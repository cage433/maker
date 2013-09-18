package maker

import maker.project.Module
import maker.utils.FileUtils._
import maker.utils.Stopwatch
import maker.utils.RichString._
import maker.utils.RichIterable._
import maker.utils.CollectionUtils._
import java.io.File


case class TestState(
  module : Module, 
  suiteClass : String, 
  test : String, 
  startTime : Long, 
  result : Option[Boolean], 
  endTime : Option[Long] = None, 
  msg : Option[String] = None, 
  exception : Option[List[String]] = None
){
  def testName = suiteClass + ":" + test
  def isComplete  = endTime.isDefined
  def time = endTime.getOrElse(throw new RuntimeException("Test " + this + " incomplete, time not defined")) - startTime
  def succeeded = (result == Some(true))
  def failed = (result == Some(false))
  def unfinished = (result == None)
  def runningTime = endTime match {
    case Some(t) => 
      t - startTime
    case None => 
      System.currentTimeMillis - startTime
  }

  def formattedFailedTest = {
    val buffer = new StringBuffer
    buffer.append("Message:\n\t" + msg.getOrElse("") + "\n")
    if (exception.isEmpty){
      buffer.append("No stack trace")
    } else {
      val highlightedThrowable = exception.get.map{
        line => if (line.contains(suiteClass)) line.inReverseRed else line.inRed
      }
      buffer.append("\nStack trace:\n")
      buffer.append(highlightedThrowable.mkString("\n\t", "\n\t", "\n"))
    }
    buffer.toString
  }

}

case class SuiteTestResult(suiteClass : String, startTime : Long, endTime : Option[Long], testResults : List[TestState]){
  def succeeded = testResults.forall(_.succeeded)
}

case class TestResults(startTime : Option[Long], endTime : Option[Long], suiteResults : List[SuiteTestResult]) {
  def succeeded = suiteResults.forall(_.succeeded)
  def ++ (rhs : TestResults) : TestResults = {
    if (rhs == TestResults.EMPTY)
      return this
    if (this == TestResults.EMPTY)
      return rhs

    val mergedStartTime = (startTime, rhs.startTime) match {
      case (Some(t1), Some(t2)) => Some(t1 min t2)
      case (Some(t1), _) => Some(t1)
      case (_, Some(t2)) => Some(t2)
      case _ => None
    }
    val mergedEndTime = (endTime, rhs.endTime) match {
      case (Some(t1), Some(t2)) => Some(t1 max t2)
      case _ => None
    }
    TestResults(
      mergedStartTime,
      mergedEndTime,
      suiteResults ::: rhs.suiteResults
    )
  }
  lazy val testResults = suiteResults.flatMap(_.testResults).sortWith(_.testName < _.testName)
  lazy val passedTests = testResults.filter(_.succeeded)
  lazy val failedTests = testResults.filter(_.failed)
  lazy val unfinishedTests = testResults.filter(_.unfinished)
  lazy val failedTestSuites = suiteResults.filterNot(_.succeeded).map(_.suiteClass)

  lazy val suites = suiteResults.map(_.suiteClass)

  def toString_ = {
    val buffer = new StringBuffer

    if (succeeded){
      buffer.append(List(
        "Number of suites", suites.size,
        "Number of tests", testResults.size,
        "Time taken " + Stopwatch.milliToHumanString(endTime.get - startTime.get)
      ).asTable(2) + "\n")
      buffer.append("Slowest test(s)\n")
      buffer.append(testResults.sortWith(_.time > _.time).take(5).flatMap{
        case ts =>
          List(ts.suiteClass, ts.test, ts.time)
      }.asTable(3) + "\n")
    } 

    if (unfinishedTests.nonEmpty){
      buffer.append("Unfinished\n")
      unfinishedTests.groupBy(_.suiteClass).map{
        case (suiteClass, tests) =>
          buffer.append("\n  " + suiteClass + "\n")
          buffer.append(tests.map(_.test).mkString("\n").indent("    "))
        }
      buffer.append("\n")
    } 

    if (failedTests.nonEmpty){
      buffer.append("Failures\n")
      var lastSuite : String = ""
      failedTests.zipWithIndex.groupBy(_._1.suiteClass)foreach{
        case (suiteClass, indexedTests) => 
          buffer.append("\n  " + suiteClass + "\n")
          indexedTests.foreach{
            case (ts, i) => 
              buffer.append("    " + ts.test + " (" + i + ")\n")
              buffer.append("      " + ts.msg.getOrElse("") + "\n")
          }
      }
      buffer.append("\ncall testResults(i) for stack trace of the i'th failing test")
    }

    buffer.toString
  } 
      
  def apply(i : Int) = {

    val tf = failedTests(i)
    println("\nDetails for " + tf.testName + "\n")
    println(tf.formattedFailedTest)
  }

  override def toString = {
    // Nasty hack to get colourization to work in the repl
    println(toString_)
    ""
  }
}

object TestResults{

  def apply(modules : List[Module]) : TestResults = modules.map(TestResults(_)).foldLeft(EMPTY)(_++_)
  def apply(module : Module) : TestResults = {

    if (! module.testResultDirectory.exists)
      return EMPTY
    
    def startTime(dir : File) = maybeFile(dir, "starttime").map(_.read.toLong)
    def endTime(dir : File) = maybeFile(dir, "endtime").map(_.read.toLong)

    val suiteResults = file(module.testResultDirectory, "suites").listFiles.toList.map{
      suiteResultsDir => 
        val suiteClass = suiteResultsDir.basename
        val testResults = file(suiteResultsDir, "tests").listFiles.toList.map{
          testResultDir => 
            val test = testResultDir.basename
            val startTime_ = startTime(testResultDir).get
            file(testResultDir, "status").read match {
              case "failed" => 
                val endTime_ = endTime(testResultDir)
                val msg = maybeFile(testResultDir, "message").map(_.read)
                val exception = maybeFile(testResultDir, "exception").map(_.readLines)
                TestState(module, suiteClass, test, startTime_, Some(false), endTime_, msg, exception)
              case "succeeded" => 
                val endTime_ = endTime(testResultDir)
                TestState(module, suiteClass, test, startTime_, Some(true), endTime_)
              case "running" => 
                TestState(module, suiteClass, test, startTime_, None)
            }
        }
        SuiteTestResult(suiteClass, startTime(suiteResultsDir).get, endTime(suiteResultsDir), testResults)

    }
    TestResults(startTime(module.testResultDirectory), endTime(module.testResultDirectory), suiteResults)
  }

  val EMPTY = TestResults(None, None, Nil)
}