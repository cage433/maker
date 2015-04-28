package maker.task.tasks

import annotation.tailrec
import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.Command
import maker.task._
import maker.task.compile.{TestCompileTask, Run, TestCompilePhase}
import maker.project.ProjectTrait
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import scala.concurrent.duration._
import scala.concurrent.Await
import maker.ConfigPimps


/**
 * run a class main in a separate JVM instance (but currently synchronously to maker repl)
 */
case class RunMainTask(
  baseProject : ProjectTrait, 
  className : String, 
  opts : Seq[String], 
  mainArgs : Seq[String],
  majorScalaVersion : String
) 
  extends Task 
  with ConfigPimps
{
  def name = "Run Main"
  import baseProject.config

  def module = baseProject
  def upstreamTasks = baseProject.testCompileTaskBuild(majorScalaVersion).tasks


  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    logger.info("running main in class " + className)


    val optsToUse = config.debugFlags ++: List(
      s"-Xmx${config.unitTestHeapSize}m", 
      "-Dlogback.configurationFile=" + "logback.xml"
    ) ++: opts
    var cmd = Command.scalaCommand(
      classpath = baseProject.testCompilationClasspath(majorScalaVersion),
      klass = className,
      opts = optsToUse,
      args = mainArgs
    )
    cmd = cmd.withOutputTo(baseProject.runMainOutputStream)
    cmd.run match {
      case 0 => DefaultTaskResult(this, true, sw)
      case code => DefaultTaskResult(this, false, sw, message = Some("Run Main failed in " + baseProject))
    }
  }
}
