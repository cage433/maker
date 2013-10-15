package maker.scalatest

import org.scalatest.Reporter
import org.scalatest.events._
import java.io._
import java.util.concurrent.atomic.AtomicBoolean

trait MakerTestReporter{
  def scalatestReporterClass : String
  def systemProperties : List[String]
  def scalatestClasspah : String
}


/**
  * Test results are written thus - with the understanding that end times will not exist until the associated
  * test run/suite/test is complete.
  *
  *  <module root>/.maker/test-results/starttime
  *                                   /endtime                                        
  *                                   /suites/<suite 1>/starttime
  *                                   /suites/<suite 1>/endtime                       
  *                                   /suites/<suite 1>/tests/<test 1>/starttime
  *                                   /suites/<suite 1>/tests/<test 1>/endtime        
  *                                   /suites/<suite 1>/tests/<test 1>/status         # one of 'running', 'failed', 'succeeded'
  *                                   /suites/<suite 1>/tests/<test 1>/msg            # Only exists if test fails
  *                                   /suites/<suite 1>/tests/<test 1>/exception      # Only exists if test fails
  *                                   /suites/<suite 1>/tests/<test 2>/starttime
  *                                   /suites/<suite 1>/tests/<test 2>/.....
  *                                   .....
  *                                   /suites/<suite 2>starttime
  *                                   /suites/<suite 2>endtime
  *                                   /suites/<suite 2>/tests/......
  */
class FileBasedMakerTestReporter extends Reporter{
  import FileUtils._
  val projectRootDirectory = Option(System.getProperty("maker.test.project.root")).getOrElse(throw new RuntimeException("Property maker.test.project.root not set"))
  val propsRootDirectory = Option(System.getProperty("maker.props.root")).getOrElse(throw new RuntimeException("Property maker.props.root not set"))

  private def recursiveDelete(file : File){
    if (file.exists && file.isDirectory){
      Option(file.listFiles).getOrElse(Array()).foreach(recursiveDelete)
      file.delete
    } else
      file.delete
  }

  val testResultsDir = new File(projectRootDirectory, ".maker/test-results")
  recursiveDelete(testResultsDir)
  testResultsDir.mkdir

  val threadDumper = new ThreadDumper(new File(propsRootDirectory), new File(projectRootDirectory))

  def suiteDir(suiteClassName : String) : File = {
    val dir = new File(testResultsDir, "suites/" + suiteClassName)
    dir.mkdirs
    dir
  }

  def testDir(suiteClass : String, test : String) : File = {
    val dir = new File(suiteDir(suiteClass), "tests/" + test)
    dir.mkdirs
    dir
  }

  def writeStartTime(dir : File, timestamp : Long){
    writeToFile(new File(dir, "starttime"), "" + timestamp)
  }

  def writeEndTime(dir : File, timestamp : Long){
    writeToFile(new File(dir, "endtime"), "" + timestamp)
  }

  def writeStatus(dir : File, state : String){
    writeToFile(new File(dir, "status"), state)
  }

  def apply(event : Event){
    event match {
      case RunStarting(ordinal, testCount, configMap, formatter, payload, threadName, timestamp) =>
        recursiveDelete(testResultsDir)
        testResultsDir.mkdir
        writeStartTime(testResultsDir, timestamp)
        threadDumper.start

      case SuiteStarting(ordinal, suiteName, suiteClassName, formatter, rerunner, payload, threadName, timestamp) => 
        val sd = suiteDir(suiteClassName.get)
        writeStartTime(sd, timestamp)

      case TestStarting(ordinal, suiteName, suiteClassName, testName, formatter, rerunner, payload, threadName, timestamp) => 
        val testDir_ = testDir(suiteClassName.get, testName)
        writeStartTime(testDir_, timestamp)
        writeStatus(testDir_, "running")

      case TestSucceeded(ordinal, suiteName, suiteClassName, testName, duration, formatter, rerunner, payload, threadName, timestamp) =>
        val testDir_ = testDir(suiteClassName.get, testName)
        writeStatus(testDir_, "succeeded")
        writeEndTime(testDir_, timestamp)
        
      case TestFailed(ordinal, message, suiteName, suiteClassName, testName, throwable, duration, formatter, rerunner, payload, threadName, timestamp) =>
        val testDir_ = testDir(suiteClassName.get, testName)
        writeStatus(testDir_, "failed")
        writeEndTime(testDir_, timestamp)
        writeToFile(new File(testDir_, "message"), message)
        val throwableAsString = throwable.map(_.getStackTrace.toList.map(_.toString)).getOrElse(Nil).mkString("\n")
        writeToFile(new File(testDir_, "exception"), throwableAsString)

      case SuiteCompleted(ordinal, suiteName, suiteClassName, duration, formatter, rerunner, payload, threadName, timestamp) => 
        val sd = suiteDir(suiteClassName.get)
        writeEndTime(sd, timestamp)

      case RunCompleted(ordinal, testCount, configMap, formatter, payload, threadName, timestamp) =>
        writeEndTime(testResultsDir, timestamp)
        threadDumper.stop

      case other =>
        
    }
  }
}

object FileUtils{
  def writeToFile(file : File, text : String){
    val fstream = new FileWriter(file, false)
    val out = new BufferedWriter(fstream)
    try {
      out.write(text)
    } finally {
      out.close()
    }
  }

  def readFromFile(file : File) : String = {
    val buffer = new StringBuffer
    val iStream = new InputStreamReader(new FileInputStream(file))
    val tmp = new Array[Char](4096)
    while(iStream.ready) {
      val read: Int = iStream.read(tmp)
      buffer.append(tmp, 0, read)
    }
    iStream.close
    buffer.toString
  }

}

/**
  * Write thread dumps on request
  */
case class ThreadDumper(propsRoot : File, moduleRoot : File){
  import FileUtils._
  import scala.collection.JavaConversions._

  private val dieSignal = new AtomicBoolean(false)
  val incomingMessageDir = new File(moduleRoot, ".maker/messages")
  val requestFile = new File(incomingMessageDir, "request")
  val requestSignalFile = new File(incomingMessageDir, "request_sent")

  val outgoingMessageDir = new File(propsRoot, ".maker/messages")
  val answerFile = new File(outgoingMessageDir, "answer")
  val answerSignalFile = new File(outgoingMessageDir, "answer_sent")

  val thread = new Thread(
    new Runnable(){

      def writeStackTraces(map : Map[Thread, Array[StackTraceElement]]){
        val buffer = new StringBuffer
        map.foreach{
          case (thread, stackTrace) =>
            buffer.append(thread.getName + "\n")
            buffer.append(
              stackTrace.toList.map(_.toString).mkString("\t", "\n\t", "\n")
            )
        }
        requestFile.delete
        requestSignalFile.delete
        writeToFile(answerFile, buffer.toString)
        writeToFile(answerSignalFile, "")
      }

      def run(){
        while(! dieSignal.get){
          try {
            if (requestSignalFile.exists && requestFile.exists){
              val request = readFromFile(requestFile)
              request.split(":").toList match {
                case List("threaddump") => 
                  writeStackTraces(Map() ++ Thread.getAllStackTraces)
                case List("threaddumpsingleclass", classname) => 
                  val traces = Map() ++ Thread.getAllStackTraces.filter{
                    case (_, trace) =>
                      trace.exists(_.toString.contains(classname))
                  }
                  writeStackTraces(traces)
                case _ =>
              }
            }
            
          } catch {
            case t: Throwable =>
              println("Debug: FileBasedMakerTestReporter: thread dumper error " + t)
          } finally {
            Thread.sleep(500)
          }
        }
      }
    }
  )

  def start{
    thread.start
  }

  def stop{
    dieSignal.set(true)
    thread.join
  }
}
