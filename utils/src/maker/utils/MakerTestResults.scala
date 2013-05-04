package maker.utils

import org.scalatest.events._
import org.scalatest.events.Event
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.SuiteStarting
import org.scalatest.Reporter
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.SynchronizedQueue
import scala.Console
import java.text.SimpleDateFormat
import java.util.Date
import java.io._
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.utils.RichIterable._
import maker.MakerProps

case class TestIdentifier(suite : String, suiteClass : String, test : String) extends Ordered[TestIdentifier]{
  def compare(rhs : TestIdentifier) = toString.compare(rhs.toString)
}
case class TestFailure(props : MakerProps, message : String, throwable : List[String]) {
  def formatted(classToHighlight : Option[String]) = {
    val buffer = new StringBuffer
    buffer.append(message + "\n\n")
    if (props.ShowFailingTestException()){
      val highlightedThrowable = classToHighlight match {
        case Some(klass) ⇒ throwable.map{
          line ⇒ if (line.contains(klass)) line.inReverseRed else line.inRed
        }
        case None ⇒ throwable
      }
      buffer.append(highlightedThrowable.asTable(1))
    }
    buffer.toString
  }
}

object MakerTestResults{
 
  def decode(s : String) : String = {
    if (s == "")
      return s
    val delimiter = s.head
    s.tail.split(delimiter).toList.mkString("\n")
  }
  def apply(props : MakerProps, file : File) : MakerTestResults = {

    val startTimeInNanos : HashMap[TestIdentifier, Long] = HashMap[TestIdentifier, Long]()
    val endTimeInNanos : HashMap[TestIdentifier, Long] = HashMap[TestIdentifier, Long]()
    val failures : HashMap[TestIdentifier, TestFailure] = HashMap[TestIdentifier, TestFailure]()
    
    withFileLineReader(file){
      line ⇒ 
        val split = line.split("\t").toList.map(decode)
        try {
        split.head match {
          case "START" ⇒ 
            val List(suite, suiteClass, test, time) = split.tail
            startTimeInNanos += TestIdentifier(suite, suiteClass, test) → time.toLong
          case "END" ⇒ 
            val List(suite, suiteClass, test, time) = split.tail
            endTimeInNanos += TestIdentifier(suite, suiteClass, test) → time.toLong
          case "FAILURE" ⇒ 
            val suite :: suiteClass :: test :: message :: throwable = split.tail
            failures += TestIdentifier(suite, suiteClass, test) → TestFailure(props, message, throwable)
        }
      } catch {
        case e ⇒
          error("problem with test-output file, file is: " + file.getAbsolutePath)
          error("head was [" + split.head + "]")
          error("split was [" + split.mkString("\n") + "]")
          error("line was [" + line + "]")
          throw e
      }
    }
    MakerTestResults(startTimeInNanos, endTimeInNanos, failures)
  }

  def outputFile : File = Option(System.getProperty("maker.test.output")) match {
    case Some(f) ⇒ new File(f)
    case None ⇒ throw new Exception(" maker must have maker.test.output set")
  }

  // Note we don't use log4j here as this runs in the user's classpath.
  val showTestProgress = java.lang.Boolean.parseBoolean(Option(System.getProperty("maker.show.test.progress")).getOrElse("true"))
  val dateFormat = new SimpleDateFormat("HH:mm:ss")
  def indent : String = {
    Option(System.getProperty("maker.level")).getOrElse("0").toInt match {
      case 0 ⇒ ""
      case i ⇒ " " * 8 * i + "L" + i + ": "
      }
  }
  def info(msg : String){
    if (showTestProgress)
      Console.err.println(indent + dateFormat.format(new Date()) + " " + msg)
  }
  def error(msg : String){
    if (showTestProgress)
      Console.err.println("\033[1;31m" + indent + dateFormat.format(new Date()) + " " + msg + "\033[0m")

  }
}


case class MakerTestResults (

  startTimeInNanos : HashMap[TestIdentifier, Long] = new HashMap[TestIdentifier, Long]() with SynchronizedMap[TestIdentifier, Long],
  endTimeInNanos : HashMap[TestIdentifier, Long] = new HashMap[TestIdentifier, Long]() with SynchronizedMap[TestIdentifier, Long],
  failures : HashMap[TestIdentifier, TestFailure] = new HashMap[TestIdentifier, TestFailure]() with SynchronizedMap[TestIdentifier, TestFailure]
) extends TaskInfo {
  import MakerTestResults._

  def failingSuiteClasses = {
    val failuresWithNoKnownClass = failures.filterKeys{
      case TestIdentifier(_, "", _) ⇒ true
      case _ ⇒ false
    }
    if (! failuresWithNoKnownClass.isEmpty){
      error("Unknown suite classes in " + failuresWithNoKnownClass.keys.toList.mkString("\n\t", "\n\t", ""))
    }
    failures.keys.map(_.suiteClass).toSet.filterNot(_ == "")
  }
  def failedTests = failures.keySet
  def passedTests = endTimeInNanos.keySet

  def ++ (rhs : MakerTestResults) = MakerTestResults(
    startTimeInNanos ++ rhs.startTimeInNanos,
    endTimeInNanos ++ rhs.endTimeInNanos,
    failures ++ rhs.failures
  )

  def succeeded = failures.isEmpty && startTimeInNanos.size == endTimeInNanos.size
  def failed = !succeeded
  def suites = startTimeInNanos.keySet.map(_.suiteClass)
  def tests = startTimeInNanos.keySet.map(_.test)
  def endTime :Long = endTimeInNanos.values.toList.sortWith(_>_).headOption.getOrElse(0L)
  def startTime :Long = endTimeInNanos.values.toList.sortWith(_<_).headOption.getOrElse(0L)
  def time = (endTime - startTime) / 1.0e9
  def failingSuites = failures.keySet.map(_.suiteClass)
  def failingTests(suite : String) = failures.keySet.filter(_.suiteClass == suite).map(_.test)

  def testsOrderedByTime : List[(TestIdentifier, Long)] = endTimeInNanos.map{
    case (id, endTime) ⇒ 
      (id, endTime - startTimeInNanos(id))
    }.toList.sortWith(_._2 > _._2)
    
  def toString_ = {
    val buffer = new StringBuffer
    if (succeeded){
      buffer.append(List(
        "Number of suites", suites.size,
        "Number of tests", tests.size,
        "Time taken (s)", "%.2f" % time
      ).asTable(2) + "\n")
      buffer.append("Slowest test(s)\n")
      buffer.append(testsOrderedByTime.take(3).flatMap{
        case (TestIdentifier(_, suiteClass, test), timeInNanos) ⇒
          List(suiteClass, test, "%.2f (s)" % (timeInNanos / 1.0e9))
      }.asTable(3) + "\n")
    } else {
      buffer.append("Failing tests\n")
      failingSuites.foreach{
        suite ⇒ 
          buffer.append("\n" + suite + "\n")
          buffer.append(failingTests(suite).map("\t\"" + _ + "\"").inAlphabeticalOrder.asTable(1) + "\n")
      }
    }
    buffer.toString
  } 
      
  override def toString = {
    // Nasty hack to get colourization to work in the repl
    println(toString_)
    ""
  }
  
  def toShortString = toString_
  def toLongString = toString_

}
