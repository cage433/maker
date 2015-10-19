package maker.utils

import org.scalatest.events._
import org.scalatest.Reporter
import scala.collection.mutable.{HashMap, SynchronizedMap, SynchronizedQueue}
import scala.Console
import scala.util.Properties
import java.text.SimpleDateFormat
import java.util.Date
import java.io._
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.utils.RichIterable._
import maker.project.ProjectTrait
import maker.task.tasks.RunUnitTestsTask


case class TestIdentifier(suite : String, suiteClass : String, test : String) extends Ordered[TestIdentifier]{
  def compare(rhs : TestIdentifier) = toString.compare(rhs.toString)
}
case class TestFailure(message : String, throwable : List[String]) {
  def formatted(classToHighlight : String) = {
    val buffer = new StringBuffer
    buffer.append("Message:\n\t" + message + "\n")
    if (throwable.isEmpty){
      buffer.append("No stack trace")
    } else {
      val highlightedThrowable = throwable.map{
        line => if (line.contains(classToHighlight)) line.inReverseRed else line.inRed
      }
      buffer.append("\nStack trace:\n")
      buffer.append(highlightedThrowable.mkString("\n\t", "\n\t", "\n"))
    }
    buffer.toString
  }

}

object MakerTestResults{

  def splitFields(line : String) : List[String] = {
    val fieldSep = 28.toChar.toString
    line.split(fieldSep).toList
  }

  def splitGroups(field : String) : String = {
    val grpSep = 29.toChar.toString
    field.split(grpSep).toList.mkString("\n")
  }

  def apply(project: ProjectTrait): MakerTestResults = apply(RunUnitTestsTask.testOutputFile(project))
  def apply(file : File): MakerTestResults = {

    val startTimeInNanos : HashMap[TestIdentifier, Long] = HashMap[TestIdentifier, Long]()
    val endTimeInNanos : HashMap[TestIdentifier, Long] = HashMap[TestIdentifier, Long]()
    var failures : List[(TestIdentifier, TestFailure)] = Nil
    
    withFileLineReader(file){
      line =>
        val fields = splitFields(line).map(splitGroups)
        try {
        fields.head match {
          case "START" => 
            val List(suite, suiteClass, test, time) = fields.tail
            startTimeInNanos += TestIdentifier(suite, suiteClass, test) -> time.toLong
          case "END" => 
            val List(suite, suiteClass, test, time) = fields.tail
            endTimeInNanos += TestIdentifier(suite, suiteClass, test) -> time.toLong
          case "FAILURE" => 
            val suite :: suiteClass :: test :: message :: throwable = fields.tail
            failures ::= (TestIdentifier(suite, suiteClass, test), TestFailure(message, throwable))
        }
      } catch {
        case e: Throwable ⇒
          error("problem with test-output file, file is: " + file.getAbsolutePath)
          error("head was [" + fields.head + "]")
          error("split was [" + fields.mkString("\n") + "]")
          error("line was [" + line + "]")
          throw e
      }
    }
    MakerTestResults(startTimeInNanos, endTimeInNanos, failures.sortWith(_._1 < _._1))
  }

  def outputFile : File = Properties.propOrNone("maker.test.output") match {
    case Some(f) => new File(f)
    case None => throw new Exception(" maker must have maker.test.output set")
  }

  // Note we don't use log4j here as this runs in the user's classpath.
  val showTestProgress = java.lang.Boolean.parseBoolean(Properties.propOrElse("maker.show.test.progress", "true"))
  val dateFormat = new SimpleDateFormat("HH:mm:ss")
  def indent : String = {
    Properties.propOrElse("maker.level", "0").toInt match {
      case 0 => ""
      case i => " " * 8 * i + "L" + i + ": "
    }
  }
  def info(msg : String){
    if (showTestProgress)
      Console.err.println(indent + dateFormat.format(new Date()) + " " + msg)
  }
  def error(msg : String){
    if (showTestProgress)
      Console.err.println((indent + dateFormat.format(new Date()) + " " + msg).inLightRed)

  }
}


case class MakerTestResults (

  startTimeInNanos : HashMap[TestIdentifier, Long] = new HashMap[TestIdentifier, Long]() with SynchronizedMap[TestIdentifier, Long],
  endTimeInNanos : HashMap[TestIdentifier, Long] = new HashMap[TestIdentifier, Long]() with SynchronizedMap[TestIdentifier, Long],
  failures : List[(TestIdentifier, TestFailure)] = Nil
) {
  import MakerTestResults._


  def failingTestIDs = failures.map(_._1)

  def passedTests = endTimeInNanos.keySet
  def failedTests = failures.map(_._1)
  

  def ++ (rhs : MakerTestResults) = MakerTestResults(
    startTimeInNanos ++ rhs.startTimeInNanos,
    endTimeInNanos ++ rhs.endTimeInNanos,
    (failures ::: rhs.failures).sortWith(_._1 < _._1)
  )

  def succeeded = failures.isEmpty && startTimeInNanos.size == endTimeInNanos.size
  def failed = !succeeded
  def unfinished = startTimeInNanos.keySet -- (endTimeInNanos.keySet ++ failures.map(_._1)) 
  def suites = startTimeInNanos.keySet.map(_.suiteClass)
  def tests = startTimeInNanos.keySet.map(_.test)
  def endTime :Long = endTimeInNanos.values.toList.sortWith(_>_).headOption.getOrElse(0L)
  def startTime :Long = endTimeInNanos.values.toList.sortWith(_<_).headOption.getOrElse(0L)
  def time = (endTime - startTime) / 1.0e9
  def failingSuiteClasses = failingTestIDs.map(_.suiteClass).distinct.filterNot(_ == "")

  def testsOrderedByTime : List[(TestIdentifier, Long)] = {
    endTimeInNanos.map{
      case (id, endTime) => 
        (id, endTime - startTimeInNanos(id))
      }.toList.sortWith(_._2 > _._2)
  }

  def orderedSuiteTimes : List[(String, Long, Long, Int)] /*(suite, clock time, cpu time, num tests) */ = {
    var testTimesBySuite = Map[String, List[(Long, Long)]]()

    endTimeInNanos.foreach{
      case (id, testEndTime) =>
        val testStartTime = startTimeInNanos(id)
        testTimesBySuite += (id.suite -> ((testStartTime, testEndTime) :: testTimesBySuite.getOrElse(id.suite, Nil)))
    }
    testTimesBySuite.map{
      case (suite, testTimes) => 
        val (clockTime, cpuTime) = Timings(testTimes).clockAndCPUTime
        (suite, clockTime, cpuTime, testTimes.size)
    }.toList.sortWith(_._2 > _._2)

  }

  def toString_ = {
    val buffer = new StringBuffer
    if (succeeded){
      buffer.append(List(
        "Number of suites", suites.size,
        "Number of tests", tests.size,
        "Time taken (s)", s"""${String.format("%.2f", new java.lang.Double(time))}"""
      ).asTable(2) + "\n")
      buffer.append("Slowest test(s)\n")
      buffer.append(testsOrderedByTime.take(3).flatMap{
        case (TestIdentifier(_, suiteClass, test), timeInNanos) =>
          List(suiteClass, test, s"""${String.format("%.2f (s)", new java.lang.Double(timeInNanos / 1.0e9))}""")
      }.asTable(3) + "\n")
    } else {
      if (unfinished.nonEmpty){
        buffer.append("Unfinished\n")
        var lastSuite : String = ""
        unfinished.foreach{
          case TestIdentifier(suite, _, test) => 
            if (suite != lastSuite){
              buffer.append("\n  " + suite + "\n")
              lastSuite = suite
            }
            buffer.append("    " + test + "\n")
        }
        buffer.append("\n")
      } 
      if (failures.nonEmpty){
        buffer.append("Failures\n")
        var lastSuite : String = ""
        failures.zipWithIndex.foreach{
          case ((TestIdentifier(suite, _, test), TestFailure(msg, _)), i) => 
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
      
  override def toString = {
    // Nasty hack to get colourization to work in the repl
    println(toString_)
    ""
  }

  def apply(i : Int) = {
    val (TestIdentifier(_, suiteClass, test), testFailure) = failures(i)
    println("\nDetails for test: " + test + "\n")
    println(testFailure.formatted(suiteClass))
  }

  
}
