package maker.utils

import org.scalatest.Reporter
import java.io._
import java.text.SimpleDateFormat
import scala.Console
import org.scalatest.events._
import java.util.Date


class MakerTestReporter extends Reporter{
  val outputFile : File = Option(System.getProperty("maker.test.output")) match {
    case Some(f) ⇒ new File(f)
    case None ⇒ throw new Exception(" maker must have maker.test.output set")
  }

  object Log{
    val showTestProgress = java.lang.Boolean.parseBoolean(Option(System.getProperty("maker.show.test.progress")).getOrElse("false"))
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
  private def replaceTabs(s : String) = s.replace("\t", "  ")
  def encode(s : String) = {
    val sansTabs = replaceTabs(s)
    "£$%^&*()!;:?+=[]{}#~@".toList.find{c ⇒ ! sansTabs.contains(c)} match {
      case Some(c) ⇒ c + sansTabs.replace('\n', c)
      case None ⇒ sansTabs
    }

  }

  private def appendToOutputFile(words : String*){
    val fstream = new FileWriter(outputFile, true)
    val out = new BufferedWriter(fstream)
    try {
      val lineToWrite = words.map(encode).mkString("", "\t", "\n")
      out.write(lineToWrite)
    } finally {
      out.close
    }
  }

  def apply(event : Event){
    val time = System.nanoTime
    event match {
      case t : SuiteStarting ⇒ 
        Log.info("Starting   " + t.suiteClassName.getOrElse(t.suiteName))
      case t : SuiteCompleted ⇒ 
        Log.info("Completed  " + t.suiteClassName.getOrElse(t.suiteName))
      case t : TestStarting ⇒ {
        appendToOutputFile("START", t.suiteName, t.suiteClassName.getOrElse(""), t.testName, time.toString)
      }
      case t : TestSucceeded ⇒ {
        appendToOutputFile("END", t.suiteName, t.suiteClassName.getOrElse(""), t.testName, time.toString)
      }
      case t : TestFailed ⇒ {
        val throwableAsList : List[String] = t.throwable.map(stackTraceAsList).getOrElse(List[String](" ")) // mkstring/split hack
        appendToOutputFile("FAILURE" :: t.suiteName :: t.suiteClassName.getOrElse("") :: t.testName :: t.message :: throwableAsList : _*)
      }
      case t : RunAborted ⇒ {
        Log.fatal("Run aborted" + t.message + t.throwable.map(stackTraceAsList).getOrElse(List[String]()).mkString("\n\t", "\n\t", ""))
      }
      case e ⇒  //Console.err.println(e)
    }
  }
}


