package maker

import java.io._
import maker.utils.FileUtils
import maker.task.compile._
import maker.project.{Module, Project}
import scala.language.reflectiveCalls
import java.util.concurrent.atomic.AtomicReference
import org.apache.commons.exec.{ExecuteResultHandler, DefaultExecuteResultHandler}
import maker.utils.os.Command
import java.util.UUID
import org.apache.commons.io.output.TeeOutputStream

case class TestMakerRepl(rootDirectory: File, teeOutput: Boolean = false) extends FileUtils with Log {

  TestMakerRepl.writeLogbackFile(rootDirectory)

  private val inputStream = TestMakerRepl.inputStream()
  private val resultHandler = new DefaultExecuteResultHandler()
  private val makerOutputFile = file(rootDirectory, "maker-output.log")

  private val os = new BufferedOutputStream(new FileOutputStream(makerOutputFile))
  def makerOutput(): String = {
    os.flush()
    readLines(makerOutputFile).mkString("\n")
  }

  private def launch() {
    val streamHandler = if (teeOutput)
      new ReplTestPumpStreamHandler(
        new TeeOutputStream(System.err, os), 
        new TeeOutputStream(System.err, os), 
        inputStream
      )
    else
      new ReplTestPumpStreamHandler(os, os, inputStream)

    val cmd = Command(
      overrideWorkingDirectory = Some(rootDirectory),
      timeout = None, 
      overrideStreamHandler = Some(streamHandler),
      args = Seq(
        file("maker.py").getAbsolutePath,
        "-l", "logback.xml",
        "-p", "Project.scala",
        "-z")
    )
    cmd.runAsync(resultHandler)
  }
  launch()

  def inputLine(line: String) = {
    inputStream.inputLine(line)
    waitForRepl()
  }

  def waitForExit() {
    resultHandler.waitFor()
  }

  def exitValue(): Int = {
    waitForExit()
    resultHandler.getExitValue()
  }

  def exit(exitValue: Int = 0) {
    inputStream.inputLine(s"System.exit($exitValue)")
    waitForExit()
  }

  def waitForRepl() {
    val file = new java.io.File(rootDirectory, UUID.randomUUID().toString)
    inputStream.inputLine(s"""new java.io.File("${file.getAbsolutePath}").createNewFile()""")
    while (! file.exists()) {
      Thread.sleep(100)
    }
    file.delete
  }

  def value(text: String): String = {

    val file = new java.io.File(rootDirectory, UUID.randomUUID().toString)
    inputStream.inputLine(s"""writeToFile(file("${file.getAbsolutePath}"), "" + $text)""")
    waitForRepl()
    val result = readLines(file).mkString("\n")
    file.delete
    result

  }
}

object TestMakerRepl extends FileUtils  {

  private def inputStream() = {
    val stream = new InputStream(){

      private var pointer: Int = 0
      var text: String = ""

      override def available() = text.size - pointer

      override def read() = {

        if (available() == 0) {
          -1
        } else {

          val c = text(pointer.toInt)
          pointer += 1
          c
        }
      }

      def inputLine(line: String) {
        text = text + s"$line\n"
      }
    }
    new BufferedInputStream(stream){
      def inputLine(line: String) {
        stream.inputLine(line)
      }
    }
  }

  def writeLogbackFile(rootDirectory: File) {
    val outputFilePath = file(rootDirectory, "maker-output.log").getAbsolutePath
    writeToFile(
      file(rootDirectory, "logback.xml"),
      s"""
<configuration scan="true" scanPeriod="3 seconds">

  <appender name="FILE" class="ch.qos.logback.core.FileAppender">
    <file>${outputFilePath}</file>
    <append>true</append>
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>
      <immediateFlush>true</immediateFlush>
    </encoder>
    <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
      <level>INFO</level>
    </filter>
  </appender>
  <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>
      <immediateFlush>true</immediateFlush>
    </encoder>
    <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
      <level>WARN</level>
    </filter>
  </appender>
        
  <root level="warn">
    <appender-ref ref="FILE" />
    <appender-ref ref="CONSOLE" />
  </root>
 </configuration>
 """
    )
  }

  def writeProjectFile(rootDirectory: File, moduleDefs: String) {
    writeToFile(
      file(rootDirectory, "Project.scala"),
      s"""
        import maker.project._
        import maker.utils.FileUtils._
        import maker.utils.FileUtils
        import maker.ScalaVersion
        import maker.task.compile._
        import org.eclipse.aether.util.artifact.JavaScopes._

        $moduleDefs

        val valueFile = new java.io.File("${file(rootDirectory, "valueFile").getAbsolutePath}")
        def writeValue(v: => Any) {
          valueFile.delete
          writeToFile(valueFile, v.toString)
        }
      """
    )
  }
}
