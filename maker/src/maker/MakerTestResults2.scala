package maker

import maker.project.Module
import maker.utils.FileUtils._
import maker.utils.TaskInfo
import maker.utils.Stopwatch
import maker.utils.RichString._
import maker.utils.RichIterable._
import maker.utils.CollectionUtils._
import java.io.File


sealed trait TestResult{
  def module : Module
  def suiteClass : String
  def test : String
  def startTime : Long
  def testName = suiteClass + ":" + test
}

trait CompletedTest extends TestResult{
  def endTime : Long
  def time = endTime - startTime
}

case class TestSucceeded(module : Module, suiteClass : String, test : String, startTime : Long, endTime : Long) extends CompletedTest
case class TestFailed(module : Module, suiteClass : String, test : String, startTime : Long, endTime : Long, msg : String, exception : List[String]) extends CompletedTest{
  def formatted(classToHighlight : String) = {
    val buffer = new StringBuffer
    buffer.append("Message:\n\t" + msg + "\n")
    if (exception.isEmpty){
      buffer.append("No stack trace")
    } else {
      val highlightedThrowable = exception.map{
        line ⇒ if (line.contains(classToHighlight)) line.inReverseRed else line.inRed
      }
      buffer.append("\nStack trace:\n")
      buffer.append(highlightedThrowable.mkString("\n\t", "\n\t", "\n"))
    }
    buffer.toString
  }
}

case class TestStillRunning(module : Module, suiteClass : String, test : String, startTime : Long) extends TestResult

case class SuiteTestResult(suiteClass : String, startTime : Long, endTime : Option[Long], testResults : List[TestResult]){
  def succeeded = testResults.forall{
    case _ : TestSucceeded => true
    case _ => false
  }
}

case class MakerTestResults2(startTime : Option[Long], endTime : Option[Long], suiteResults : List[SuiteTestResult]) extends TaskInfo{
  def succeeded = suiteResults.forall(_.succeeded)
  def ++ (rhs : MakerTestResults2) = {
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
    MakerTestResults2(
      mergedStartTime,
      mergedEndTime,
      suiteResults ::: rhs.suiteResults
    )
  }
  lazy val testResults = suiteResults.flatMap(_.testResults)
  lazy val passedTests = filterOnType[TestSucceeded](testResults).toList
  lazy val failedTests = filterOnType[TestFailed](testResults).toList
  lazy val unfinishedTests = filterOnType[TestStillRunning](testResults).toList
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
      buffer.append(passedTests.sortWith(_.time > _.time).take(3).flatMap{
        case tr @ TestSucceeded(_, suiteClass, test, _, _) => 
          List(suiteClass, test, tr.time)
      }.asTable(3) + "\n")
    } else {
      if (unfinishedTests.nonEmpty){
        buffer.append("Unfinished\n")
        var lastSuite : String = ""
        unfinishedTests.foreach{
          case tr @ TestStillRunning(_, suite, test, _) => 
            if (suite != lastSuite){
              buffer.append("\n  " + suite + "\n")
              lastSuite = suite
            }
            buffer.append("    " + test + "\n")
        }
        buffer.append("\n")
      } 
      if (failedTests.nonEmpty){
        buffer.append("Failures\n")
        var lastSuite : String = ""
        failedTests.zipWithIndex.foreach{
          case (TestFailed(_, suite, test, _, _, msg, exception), i) => 
            if (suite != lastSuite){
              buffer.append("\n  " + suite + "\n")
              lastSuite = suite
            }
            buffer.append("    " + test + " (" + i + ")\n")
            buffer.append("      " + msg + "\n")
        }
        buffer.append("\ncall testResults(i) for stack trace of the i'th failing test")
      }
    }
    buffer.toString
  } 
      
  def apply(i : Int) = {

    val tf@TestFailed(_, suiteClass, test, startTime, endTime, msg, exception) = failedTests(i)
    println("\nDetails for " + tf.testName + "\n")
    println(tf.formatted(suiteClass))
  }

  override def toString = {
    // Nasty hack to get colourization to work in the repl
    println(toString_)
    ""
  }
  
  def toShortString = toString_
  def toLongString = toString_
}

object MakerTestResults2{

  def apply(modules : List[Module]) : MakerTestResults2 = modules.map(MakerTestResults2(_)).reduce(_++_)
  def apply(module : Module) : MakerTestResults2 = {

    if (! module.testResultDirectory.exists)
      return MakerTestResults2(None, None, Nil)
    
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
                val endTime_ = endTime(testResultDir).get
                val msg = maybeFile(testResultDir, "message").map(_.read).getOrElse("")
                val exception = maybeFile(testResultDir, "exception").map(_.readLines).getOrElse(Nil)
                TestFailed(module, suiteClass, test, startTime_, endTime_, msg, exception)
              case "succeeded" => 
                val endTime_ = endTime(testResultDir).get
                TestSucceeded(module, suiteClass, test, startTime_, endTime_)
              case "running" => 
                TestStillRunning(module, suiteClass, test, startTime_)
            }
        }
        SuiteTestResult(suiteClass, startTime(suiteResultsDir).get, endTime(suiteResultsDir), testResults)

    }
    MakerTestResults2(startTime(module.testResultDirectory), endTime(module.testResultDirectory), suiteResults)
  }
}
