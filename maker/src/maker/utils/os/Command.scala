package maker.utils.os

import java.io.{InputStream, OutputStream, File}
import org.apache.commons.exec._
import org.apache.commons.io.output.NullOutputStream.NULL_OUTPUT_STREAM
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory
import maker.ConfigPimps

case class Command(
  overrideOutput : Option[OutputStream],
  timeout : Option[Duration],
  overrideWorkingDirectory : Option[File] = None,
  overrideExitValues : Option[Seq[Int]] = None,
  args : Seq[String]
){
  override def toString = args.mkString(" ")

  def withOutputTo(os : OutputStream) = copy(overrideOutput = Some(os))

  def withNoOutput = withOutputTo(NULL_OUTPUT_STREAM)

  def withWorkingDirectory(dir : File) = copy(overrideWorkingDirectory = Some(dir))

  def withExitValues(values : Int*) = copy(overrideExitValues = Some(values))

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
    overrideExitValues.foreach{values => executor_.setExitValues(values.toArray)}
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

object Command extends ConfigPimps{
  def apply(args : String*) : Command = Command(
    overrideOutput = None, 
    timeout = None,
    overrideWorkingDirectory = None,
    overrideExitValues = None,
    args 
  )

  def scalaCommand(classpath : String, klass : String, opts : Seq[String] = Nil, args : Seq[String] = Nil) : Command = {
    val config = ConfigFactory.load()

    var commandArgs : Seq[String] =  
      config.javaExecutable.getAbsolutePath +: 
      opts ++:
      List("-Dscala.usejavacp=true", "-classpath", classpath, "scala.tools.nsc.MainGenericRunner", klass) ++: 
      args

    Command(commandArgs : _*)
  }
}
