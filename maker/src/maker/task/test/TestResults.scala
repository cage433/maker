package maker.task.test

import maker.project.Module
import maker.utils.FileUtils._
import maker.utils.Stopwatch
import maker.utils.Implicits.RichString._
import maker.utils.Implicits.RichIterable._
import java.io.File
import maker.project.BaseProject
import maker.project.Project
import org.scalatest.events.Event
import org.scalatest.events._

case class TestResults(eventsByModule : Map[String, List[Event]]) {
  def ++(rhs : TestResults) = TestResults(eventsByModule ++ rhs.eventsByModule)
  val events = eventsByModule.values.flatten

  def succeeded : Boolean = isComplete && numFailedTests == 0

  def isComplete = eventsByModule.values.forall {
    event => events.collect{case _ : RunCompleted => true}.nonEmpty
  }

  def numPassedTests = events.collect {
    case _ : TestSucceeded => true
  }.size

  def numFailedTests = events.collect {
    case _ : TestFailed => true
  }.size

  def failedTestSuites = events.collect {
    case t : TestFailed => t.suiteClassName.get
  }.toList.distinct
  override def toString = "TestResults " + succeeded
}


object TestResults{
  val EMPTY = TestResults(Map.empty)
}
