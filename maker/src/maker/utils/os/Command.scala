package maker.utils.os

import java.io.{InputStream, OutputStream, File}
import org.apache.commons.exec._
import org.apache.commons.io.output.NullOutputStream.NULL_OUTPUT_STREAM
import scala.concurrent.duration.Duration
import maker.{MakerConfig, Log}
import org.slf4j.LoggerFactory
import maker.utils.Int

case class Command(
  overrideOutput : Option[OutputStream],
  timeout : Option[Duration],
  overrideWorkingDirectory : Option[File] = None,
  overrideExitValues : Option[Seq[Int]] = None,
  args : Seq[String]
) 
  extends Log
{
  override def toString = args.mkString(" ")

  def withOutputTo(os : OutputStream) = copy(overrideOutput = Some(os))

  def withTimeout(timeout : Duration) = copy(timeout = Some(timeout))

  def withNoOutput = withOutputTo(NULL_OUTPUT_STREAM)

  def withWorkingDirectory(dir : File) = copy(overrideWorkingDirectory = Some(dir))

  def withExitValues(values : Int*) = copy(overrideExitValues = Some(values))

  def commandLine = {
    val cmd = new CommandLine(args.head)
    args.tail.foreach(cmd.addArgument(_))
    cmd
  }

  val watchdog = {
    val timeoutMillis = timeout.map(_.toMillis).getOrElse(ExecuteWatchdog.INFINITE_TIMEOUT)
    new ExecuteWatchdog(timeoutMillis)
  }
  def executor = {
    val streamHandler = overrideOutput match {
      case Some(os) => new PumpStreamHandler(os)
      case None => new PumpStreamHandler()
    }

    val executor_ = new DefaultExecutor()
    executor_.setStreamHandler(streamHandler)
    executor_.setWatchdog(watchdog)
    overrideWorkingDirectory.foreach(executor_.setWorkingDirectory(_))
    overrideExitValues.foreach{values => executor_.setExitValues(values.toArray)}
    executor_
  }
  def run() = {
    val result = executor.execute(commandLine)
    if (watchdog.killedProcess)
      logger.error(s"Command '${toString}' timed out after $timeout")
    result
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
    logger.info(s"running command '${toString}' asynchronously")
    executor.execute(commandLine, resultHandler)
  }
}

object Command {
  def apply(args : String*) : Command = Command(
    overrideOutput = None, 
    timeout = None,
    overrideWorkingDirectory = None,
    overrideExitValues = None,
    args 
  )

  def scalaCommand(config: MakerConfig, classpath : String, klass : String, opts : Seq[String] = Nil, args : Seq[String] = Nil) : Command = {

    var commandArgs : Seq[String] =  
      config.javaExecutable.getAbsolutePath +: 
      opts ++:
      List("-Dscala.usejavacp=true", "-classpath", classpath, "scala.tools.nsc.MainGenericRunner", klass) ++: 
      args

    Command(commandArgs : _*)
  }
}
