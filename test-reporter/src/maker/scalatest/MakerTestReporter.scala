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
  lazy val messagingDir : File = Properties.propOrNone("maker.testreporter.messaging.directory" ) match {
    case Some(f) => new File(f)
    case None => throw new Exception(" maker must have maker.testreporter.messaging.directory set")
  }

  private val EscapeChars = sys.env.get("MAKER_ESCAPECHARS").isDefined
  private def inColour(code: String, s: String) =
    if (EscapeChars) "\u033b[" + code + "m" + s + "\u033b[0m" else s
  private val Green = "0;32"
  private val Red = "0;31"
  private val LightRed = "1;31"

  object Log{
    def info(msg : String){
      Console.err.println("[info] " + msg)
    }
    def error(msg : String){
      Console.err.println("[warn] " + inColour(LightRed, msg))
    }
    def fatal(msg : String){
      Console.err.println("[error] " + inColour(LightRed, msg))
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
        //Log.info("STARTING " + t.suiteClassName.getOrElse(t.suiteName))
      case t : SuiteCompleted => 
        //Log.info("Completed  " + t.suiteClassName.getOrElse(t.suiteName))
      case t : TestStarting => {
        appendToOutputFile("START", t.suiteName, t.suiteClassName.getOrElse(""), t.testName, time.toString)
      }
      case t : TestSucceeded => {
        //Log.info(inColour(Green, t.suiteName + "/" + t.testName.take(50)))
        appendToOutputFile("END", t.suiteName, t.suiteClassName.getOrElse(""), t.testName, time.toString)
      }
      case t : TestFailed => {
        // it would be great to put the full path to the file (and line) here
        val source = t.suiteClassName match {
          case Some(clazz) => clazz
          case None => t.suiteName
        }
        Log.error(inColour(Red, "FAILED " + source + " " + t.testName))
        appendToOutputFile("FAILURE" :: t.suiteName :: t.suiteClassName.getOrElse("") :: t.testName :: t.message :: Nil : _*)
      }
      case t : RunAborted => {
        Log.fatal("Run aborted" + t.message) 
      }
      case e =>  //Console.err.println(e)
    }
  }
}


