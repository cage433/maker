package maker.utils.os

import java.io.{InputStream, OutputStream, File}
import org.apache.commons.exec._
import org.apache.commons.io.output.NullOutputStream.NULL_OUTPUT_STREAM
import scala.concurrent.duration.Duration

case class Command2(
  overrideOutput : Option[OutputStream],
  timeout : Option[Duration],
  overrideWorkingDirectory : Option[File] = None,
  args : Seq[String]
){
  def withOutputTo(os : OutputStream) = copy(overrideOutput = Some(os))
  def withNoOutput = withOutputTo(NULL_OUTPUT_STREAM)
  def withWorkingDirectory(dir : File) = copy(overrideWorkingDirectory = Some(dir))

  def commandLine = {
    val cmd = new CommandLine(args.head)
    args.tail.foreach(cmd.addArgument(_))
    cmd
  }

  def executor = {
    val streamHandler = overrideOutput match {
      case Some(os) => new PumpStreamHandler(os)
      case None => new PumpStreamHandler()
    }

    val watchdog = {
      val timeoutMillis = timeout.map(_.toMillis).getOrElse(ExecuteWatchdog.INFINITE_TIMEOUT)
      new ExecuteWatchdog(timeoutMillis)
    }
    val executor_ = new DefaultExecutor()
    executor_.setStreamHandler(streamHandler)
    executor_.setWatchdog(watchdog)
    overrideWorkingDirectory.foreach(executor_.setWorkingDirectory(_))
    executor_
  }
  def run() = {
    executor.execute(commandLine)
  }

  def runAsync() = {
    val resultHandler = new ExecuteResultHandler(){
      def onProcessComplete(exitValue : Int) = {
        // do nothing
      }
      def onProcessFailed(e : ExecuteException) = {
        // do nothing
      }
    }
    executor.execute(commandLine, resultHandler)
  }
}

object Command2{
  def apply(args : String*) : Command2 = Command2(
    overrideOutput = None, 
    timeout = None,
    overrideWorkingDirectory = None,
    args 
  )
}
