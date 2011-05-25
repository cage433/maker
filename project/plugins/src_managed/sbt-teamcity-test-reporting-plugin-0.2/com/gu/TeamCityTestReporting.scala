package com.gu

import sbt._
import org.scalatools.testing.{Event => TEvent, Result => TResult}


trait TeamCityTestReporting extends BasicScalaProject {
  override def testListeners = super.testListeners ++ TeamCityTestListener.ifRunningUnderTeamCity
}

class TeamCityTestListener extends TestReportListener {
  /** called for each class or equivalent grouping */
  def startGroup(name: String) = teamcityReport("testSuiteStarted", "name" -> name )

  /** called for each test method or equivalent */
  def testEvent(event: TestEvent) = {
    for (e: TEvent <- event.detail) {

      // this is a lie: the test has already been executed and started by this point,
      // but sbt doesn't send an event when test starts
      teamcityReport("testStarted", "name" -> e.testName)

      e.result match {
        case TResult.Success => // nothing extra to report
        case TResult.Error | TResult.Failure =>
          teamcityReport("testFailed",
            "name" -> e.testName,
            "details" -> (e.error.toString.replace("\n", " ").replace("'", "\"") + " " + e.error.getStackTrace().mkString(" at ", " at ", "")))
        case TResult.Skipped =>
          teamcityReport("testIgnored", "name" -> e.testName)
      }

      teamcityReport("testFinished", "name" -> e.testName)

    }
  }


  /** called if there was an error during test */
  def endGroup(name: String, t: Throwable) = teamcityReport("testSuiteFinished", "name" -> name)
  /** called if test completed */
  def endGroup(name: String, result: Result.Value) = teamcityReport("testSuiteFinished", "name" -> name)

  private def teamcityReport(messageName: String, attributes: (String, String)*) =
    println("##teamcity["+ messageName +" "+ attributes.map{ case (k, v) => k +"='"+ v +"'" }.mkString(" ") + "]")
}

object TeamCityTestListener {
  // this is nicer in 2.8 with Option.apply ...
  private lazy val teamCityProjectName = Some(System.getenv("TEAMCITY_PROJECT_NAME")).filter(_ != null)
  lazy val ifRunningUnderTeamCity = teamCityProjectName.map(ignore => new TeamCityTestListener)

  println("teamcityprojectname = " + teamCityProjectName)

}
