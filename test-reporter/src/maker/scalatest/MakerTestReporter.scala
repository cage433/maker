package maker.utils

import org.scalatest.Reporter
import java.io._
import java.text.SimpleDateFormat
import scala.Console
import org.scalatest.events._
import java.util.Date
import scala.util.Properties


/**
  * Writes info on the progress of tests to an output file
  * The output takes the form
  *
  * START<28>SUITE<28>SUITE CLASS<28>TEST NAME<28>TIME<\n>
  *
  * END<28>SUITE<28>SUITE CLASS<28>TEST NAME<28>TIME<\n>
  *
  * FAILURE<28>SUITE<28>SUITE CLASS<28>TEST NAME<28>MESSAGE<28>STACK TRACE 1<29>STACK TRACE 2<29>....<\n>
  *
  * where <28> and <29> are the ascii field and group separator characters
  */

class MakerTestReporter extends Reporter{
  val outputFile : File = Properties.propOrNone("maker.test.output") match {
    case Some(f) => new File(f)
    case None => throw new Exception(" maker must have maker.test.output set")
  }

  object Log{
    val showTestProgress = Properties.propOrFalse("maker.show.test.progress")
    val dateFormat = new SimpleDateFormat("mm:ss,SSS")
    def info(msg : String){
      if (showTestProgress)
        Console.err.println(dateFormat.format(new Date()) + " " + msg)
    }
    def error(msg : String){
      if (showTestProgress)
        Console.err.println("\033[1;31m" + dateFormat.format(new Date()) + " " + msg + "\033[0m")
    }
    def fatal(msg : String){
      Console.err.println("\033[1;31m" + dateFormat.format(new Date()) + " " + msg + "\033[0m")
    }
  }

  private def stackTraceAsList(t : Throwable) : List[String] = {
    val sw = new StringWriter()
    val w = new PrintWriter(sw)
    t.printStackTrace(w)
    sw.toString.split("\n").toList
  }

  def replaceNewLineWithGroupSeparator(s : String) = {
    val grpSep = 29.toChar.toString
    s.replace("\n", grpSep)
  }

  def joinWithFieldSeparator(fields : List[String]) : String = {
    val fieldSep = 28.toChar.toString
    fields.mkString("", fieldSep, "\n")
  }


  private def appendToOutputFile(words : String*){
    val fstream = new FileWriter(outputFile, true)
    val out = new BufferedWriter(fstream)
    try {
      val fields = words.toList.map(replaceNewLineWithGroupSeparator)
      val line = joinWithFieldSeparator(fields)
      out.write(line)
    } finally {
      out.close
    }
  }

  def apply(event : Event){
    val time = System.nanoTime
    event match {
      case t : SuiteStarting => 
        Log.info("Starting   " + t.suiteClassName.getOrElse(t.suiteName))
      case t : SuiteCompleted => 
        Log.info("Completed  " + t.suiteClassName.getOrElse(t.suiteName))
      case t : TestStarting => {
        appendToOutputFile("START", t.suiteName, t.suiteClassName.getOrElse(""), t.testName, time.toString)
      }
      case t : TestSucceeded => {
        appendToOutputFile("END", t.suiteName, t.suiteClassName.getOrElse(""), t.testName, time.toString)
      }
      case t : TestFailed => {
        val throwableAsList : List[String] = t.throwable.map(stackTraceAsList).getOrElse(List[String](" ")) // mkstring/split hack
        appendToOutputFile("FAILURE" :: t.suiteName :: t.suiteClassName.getOrElse("") :: t.testName :: t.message :: throwableAsList : _*)
      }
      case t : RunAborted => {
        Log.fatal("Run aborted" + t.message + t.throwable.map(stackTraceAsList).getOrElse(List[String]()).mkString("\n\t", "\n\t", ""))
      }
      case e =>  //Console.err.println(e)
    }
  }
}


