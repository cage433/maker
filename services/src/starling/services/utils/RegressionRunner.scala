package starling.services.utils

import scala.io.Source

import starling.daterange.Day
import starling.utils.ImplicitConversions._
import starling.services.StarlingInit
import starling.props.PropsHelper
import starling.http.HttpServer
import starling.concurrent.MP
import java.io._
import java.net.{HttpURLConnection, URLConnection, URL}
import starling.utils.ImplicitConversions._
import starling.auth.AuthHandler
import starling.utils.{Broadcaster, Log}


trait ReportSource {
  def name:String
  def reports:Set[String]
  def report(name:String, day:Day):String
}
class HttpReportSource(url:String) extends ReportSource {
  def name = url
  def reports = {
    val fullURL = url + "/reports/list"
    try {
      Set() ++ Source.fromURL(fullURL).getLines
    }
    catch {
      case e => {
        Log.error("Failed to get: " + fullURL, e)
        throw e
      }
    }
  }
  def report(name: String, day:Day) = {
    val reportURL = new URL(url + "/reports/" + name + "/" + day)
    Source.fromInputStream(open(reportURL, "Accept" → "text/csv")).getLines.map(_.trim).mkString("\n")
  }

  private def open(url: URL, headers: (String, String)*) = url.openConnection.asInstanceOf[HttpURLConnection].update {
    connection => headers.foreach { case (key, value) => connection.addRequestProperty(key, value) }
  }.getInputStream
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

//val prodURL = //new URL(expectedURL + commonURL)
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

object RegressionRunner {
  val defaultDay = Day(2011, 6, 24)

  def main(args:Array[String]) {
    try {
      System.setProperty("log4j.configuration", "utils/resources/log4j.properties")

      val props = PropsHelper.defaultProps
      val currentURL = "http://localhost:" + props.RegressionPort()
      //val keyFile = System.getProperty("home.dir") + "/.ssh/id_rsa"
      //SSHPortForwarding.forward(userName = "thomas.rynne", hostname = "starling", remotePort = 25848, privateKeyFile = keyFile) {
      //  port => {
          val starlingInit = new StarlingInit(props, AuthHandler.Dev, Broadcaster.Null, true, true, true, startEAIAutoImportThread=false, startXLLoop = false,
            startStarlingJMX = false, forceGUICompatability = false)
          starlingInit.start
          try {
            val result = new RegressionRunner(CheckedInReports, new HttpReportSource(currentURL)).run(System.out)
            if (!result) {
              println("##teamcity[buildStatus status='FAILURE' text='Regression failed']")
              System.out.flush
              System.exit(1) // this allows us to git bisect run to automatically find the failing version
            }
          } catch {
            case e => {
              e.printStackTrace
            }
          } finally {
            starlingInit.stop
            Thread.sleep(3000)
            printNonDaemonThreads
          }
      //  }
      //}
    } catch {
      case e:Exception => {
        println("##teamcity[buildStatus status='FAILURE' text='Regression exception']")
        e.printStackTrace
        System.exit(1)
      }
    }
  }

  def printNonDaemonThreads {
    Thread.getAllStackTraces.entrySet.toArray(Array[java.util.Map.Entry[Thread,Array[StackTraceElement]]]()).foreach {
      entry => {
        val thread = entry.getKey
        if (!thread.isDaemon && thread.getName != "main") {
          println("Non daemon thread: " + thread.getName)

        }
      }
    }
  }

}