package maker.scalatest

import org.scalatest.Reporter
import org.scalatest.events._
import java.io._

class MakerTestReporter2 extends Reporter{
  val projectRootDirectory = Option(System.getProperty("maker.test.project.root")).getOrElse(throw new RuntimeException("Property maker.test.project.root not set"))
  private def recursiveDelete(file : File){
    if (file.exists && file.isDirectory){
      Option(file.listFiles).flatten.foreach(recursiveDelete)
      file.delete
    } else
      file.delete
  }

  val testResultsDir = new File(projectRootDirectory, ".maker/test-results")
  def suiteDir(suiteClassName : String) : File = new File(testResultsDir, suiteClassName)

  def writeToFile(file : File, text : String, append : Boolean){
    val fstream = new FileWriter(file, append)
    val out = new BufferedWriter(fstream)
    try {
      out.write(text)
    } finally {
      out.close()
    }
  }

  def writeStartTime(dir : File){
    writeToFile(new File(dir, "starttime"), "" + System.currentTimeMillis, append = false)
  }

  def writeState(dir : File, state : String){
    writeToFile(new File(dir, "state"), state, append = false)
  }

  def apply(event : Event){
    event match {
      case RunStarting(ordinal, testCount, configMap, formatter, payload, threadName, timestamp) =>
        recursiveDelete(testResultsDir)
        testResultsDir.mkdir
        writeStartTime(testResultsDir)
        writeState(testResultsDir, "started")

      case SuiteStarting(ordinal, suiteName, suiteClassName, formatter, rerunner, payload, threadName, timestamp) => 
        val sd = suiteDir(suiteClassName.get)
        sd.mkdir
        writeStartTime(sd)

      case TestStarting(ordinal, suiteName, suiteClassName, testName, formatter, rerunner, payload, threadName, timestamp) => 
        val testDir = new File(suiteDir(suiteClassName.get), testName)
        testDir.mkdir
        writeStartTime(testDir)
        writeState(testDir, "started")

      case TestSucceeded(ordinal, suiteName, suiteClassName, testName, duration, formatter, rerunner, payload, threadName, timestamp) =>
        val testDir = new File(suiteDir(suiteClassName.get), testName)
        writeState(testDir, "succeeded")
        
      case TestFailed(ordinal, message, suiteName, suiteClassName, testName, throwable, duration, formatter, rerunner, payload, threadName, timestamp) =>
        val testDir = new File(suiteDir(suiteClassName.get), testName)
        writeState(testDir, "failed")

      case SuiteCompleted(ordinal, suiteName, suiteClassName, duration, formatter, rerunner, payload, threadName, timestamp) => 
        val sd = suiteDir(suiteClassName.get)
        writeState(sd, "complete")

      case RunCompleted(ordinal, testCount, configMap, formatter, payload, threadName, timestamp) =>
        writeState(testResultsDir, "complete")

      case other =>
        println("Received event " + event)
        
    }
  }
}
