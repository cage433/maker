package maker.task.tasks

import annotation.tailrec
import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.Command
import maker.task._
import maker.task.compile.CompilePhase
import maker.project.ProjectTrait
import ch.qos.logback.classic.Logger
import scala.concurrent.duration._
import scala.concurrent.Await
import maker.{ScalaVersion, Log}


/**
 * run a class main in a separate JVM instance (but currently synchronously to maker repl)
 */
case class RunMainTask(
  baseProject : ProjectTrait, 
  className : String, 
  opts : Seq[String], 
  mainArgs : Seq[String],
  scalaVersion : ScalaVersion
) 
  extends Task with Log
{
  def name = "Run Main"

  def module = baseProject
  def upstreamTasks = baseProject.compileTaskBuild(CompilePhase.TEST_PHASES).tasks


  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    logger.info("running main in class " + className)


    val optsToUse = baseProject.remoteDebuggingOption ++: List(
      s"-Xmx${baseProject.unitTestHeapSize}m", 
      "-Dlogback.configurationFile=" + Option(System.getProperty("logback.configurationFile")).getOrElse("logback.xml")
    ) ++: opts
    val outputStream = {
      val runLogFile = file(baseProject.rootAbsoluteFile, "runlog.out")
      new TeeToFileOutputStream(runLogFile)
    }
    var cmd = Command.scalaCommand(
      baseProject,
      classpath = baseProject.runtimeClasspath(CompilePhase.PHASES),
      klass = className,
      opts = optsToUse,
      args = mainArgs
    )
    cmd = cmd.withOutputTo(outputStream)
    cmd.run match {
      case 0 => DefaultTaskResult(this, true, sw)
      case code => DefaultTaskResult(this, false, sw, message = Some("Run Main failed in " + baseProject))
    }
  }
}
