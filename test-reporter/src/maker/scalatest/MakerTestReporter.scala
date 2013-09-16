package maker.scalatest

import org.scalatest.Reporter
import org.scalatest.events._
import java.io._

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
class MakerTestReporter extends Reporter{
  val projectRootDirectory = Option(System.getProperty("maker.test.project.root")).getOrElse(throw new RuntimeException("Property maker.test.project.root not set"))
  private def recursiveDelete(file : File){
    if (file.exists && file.isDirectory){
      Option(file.listFiles).flatten.foreach(recursiveDelete)
      file.delete
    } else
      file.delete
  }

  val testResultsDir = new File(projectRootDirectory, ".maker/test-results")
  recursiveDelete(testResultsDir)
  testResultsDir.mkdir

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

  def writeToFile(file : File, text : String, append : Boolean){
    val fstream = new FileWriter(file, append)
    val out = new BufferedWriter(fstream)
    try {
      out.write(text)
    } finally {
      out.close()
    }
  }

  def writeStartTime(dir : File, timestamp : Long){
    writeToFile(new File(dir, "starttime"), "" + timestamp, append = false)
  }

  def writeEndTime(dir : File, timestamp : Long){
    writeToFile(new File(dir, "endtime"), "" + timestamp, append = false)
  }

  def writeStatus(dir : File, state : String){
    writeToFile(new File(dir, "status"), state, append = false)
  }

  def apply(event : Event){
    event match {
      case RunStarting(ordinal, testCount, configMap, formatter, payload, threadName, timestamp) =>
        recursiveDelete(testResultsDir)
        testResultsDir.mkdir
        writeStartTime(testResultsDir, timestamp)

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
        writeToFile(new File(testDir_, "message"), message, append = false)
        val throwableAsString = throwable.map(_.getStackTrace.toList.map(_.toString)).getOrElse(Nil).mkString("\n")
        writeToFile(new File(testDir_, "exception"), throwableAsString, append = false)


      case SuiteCompleted(ordinal, suiteName, suiteClassName, duration, formatter, rerunner, payload, threadName, timestamp) => 
        val sd = suiteDir(suiteClassName.get)
        writeEndTime(sd, timestamp)

      case RunCompleted(ordinal, testCount, configMap, formatter, payload, threadName, timestamp) =>
        writeEndTime(testResultsDir, timestamp)

      case other =>
        
    }
  }
}
