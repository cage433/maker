package starling.startserver.apps

import starling.startserver.SingleClasspathBroadcasterActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.services.osgi.ServicesBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import starling.reports.ReportService
import starling.daterange.Day
import io.Source
import java.io.{PrintStream, File}
import starling.auth.{User}
import starling.utils.ThreadUtils
import starling.reports.osgi.ReportsBromptonActivator

object RegressionRunner {
  val defaultDay = Day(2011, 6, 24)

  def main(args:Array[String]) {
    val activators = List(
      classOf[SingleClasspathBroadcasterActivator],
      classOf[AuthBromptonActivator],
      classOf[ServicesBromptonActivator],
      classOf[ReportsBromptonActivator]
    )
    val single = new SingleClasspathManager(starling.manager.Props.readDefault, activators)
    single.start
    val reportService = single.service(classOf[ReportService])

    val result = new RegressionRunner(CheckedInReports, new ReportServiceSource(reportService)).run(System.out)
    if (!result) {
      println("##teamcity[buildStatus status='FAILURE' text='Regression failed']")
      System.out.flush
      System.exit(1) // this allows us to git bisect run to automatically find the failing version
    }
    ThreadUtils.printNonDaemonThreads
  }
}

trait ReportSource {
  def name:String
  def reports:Set[String]
  def report(name:String, day:Day):String
}
class ReportServiceSource(reportService:ReportService) extends ReportSource {
  def name = "jvm ReportService"
  def reports = {
    Set() ++ reportService.allUserReports.flatMap { case(user,reports) => {
      reports.map { report=> {
        user + "/" + report.reportName
      } }
    } }
  }
  def report(name: String, day:Day) = {
    val split = name.split("/")
    val userName = split(0)
    val reportName = split(1)
    reportService.runNamedReport(User(userName), reportName, day, None) match {
      case Some(pivotData) => pivotData.asCSV
      case None => throw new Exception("No report found named " + reportName + " " + userName)
    }
  }
}

object CheckedInReports extends ReportSource {
  def name = "checked in"
  def reports = Set() ++ Set("Thomas.Rynne/Regression-Jon", "Thomas.Rynne/Regression-Seetal")
  def report(name: String, day:Day) = {
    if (day != Day(2011, 6, 24)) throw new Exception("I commited the report for the 24th Jun")
    val resource = name.stripPrefix("Thomas.Rynne/")
    val resourceURL = new File("services/src/starling/services/utils/" + resource).toURI.toURL
    if (resourceURL == null) throw new Exception("Can't find resource: " + resource)
    Source.fromURL(resourceURL).getLines.map(_.trim).mkString("\n")
  }
}

class RegressionRunner(expected:ReportSource, actual:ReportSource) {
  def run(writer:PrintStream) = {
    writer.println("Running regression tests")
    val devReports = actual.reports
    val prodReports = expected.reports

    val commonReports = devReports & prodReports

    writer.println(expected.name+ " reports " + prodReports.size)
    writer.println(actual.name+ " reports " + devReports.size)
    writer.println("common reports " + commonReports.size)
    writer.println()
    if (commonReports.isEmpty) {
      writer.println("Failed. There are no common reports to test")
      false
    } else {
      var failures = 0
      commonReports.foreach { report => {
        val userName = report.split("/")(0)
        val reportName = report.split("/")(1)

        val prod = expected.report(report, RegressionRunner.defaultDay)
        val dev = actual.report(report, RegressionRunner.defaultDay)

        val reportsAreEqual = dev == prod
        if (!reportsAreEqual) {
          val differenceIndex = ((dev zip prod) zipWithIndex).find { case((a,b),index) => a!=b}

          writer.println(report + " FAIL")
          println(differenceIndex)
          writer.println("DEV ")// + println(dev.substring(differenceIndex, differenceIndex+10)))
          writer.println(dev)
          writer.println()
          writer.println("PROD ")// + println(prod.substring(differenceIndex, differenceIndex+10)))
          writer.println(prod)
          writer.println()
          failures +=1
        } else {
          writer.println(report + " OK")
        }
      } }
      writer.println()
      if (failures == 0) {
        writer.println("OK")
      } else {
        writer.println("Failed with " + failures + " failures")
      }
      failures == 0
    }
  }
}