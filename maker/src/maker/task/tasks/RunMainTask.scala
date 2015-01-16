package maker.task.tasks

import annotation.tailrec
import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.{CommandOutputHandler, ScalaCommand}
import maker.task._
import maker.task.compile.TestCompileTask
import maker.project.BaseProject
import maker.utils.Utils.debuggerFlagsFromPortFile
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import scala.concurrent.duration._
import scala.concurrent.Await


/**
 * run a class main in a separate JVM instance (but currently synchronously to maker repl)
 */
case class RunMainTask(baseProject : BaseProject, className : String, opts : List[String], mainArgs : List[String]) extends Task {
  def name = "Run Main"

  def module = baseProject
  def upstreamTasks = baseProject.allUpstreamModules.map(TestCompileTask(_))


  val runLogFile = file(baseProject.rootAbsoluteFile, "runlog.out")
  val logger = LoggerFactory.getLogger(this.getClass)
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = baseProject.props
    logger.info("running main in class " + className)

    val writer = new PrintWriter(new TeeToFileOutputStream(runLogFile))

    val debugArguments = debuggerFlagsFromPortFile(props.DebugPortMain())

    val optsToUse = debugArguments ::: List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=200m",
      "-Dlogback.configurationFile=" + "logback.xml"
    ) ::: opts
    val cmd = ScalaCommand(
      props,
      new CommandOutputHandler(Some(writer)),
      props.Java().getAbsolutePath,
      optsToUse,
      baseProject.testClasspath,
      className,
      "Running main in " + baseProject.name,
      mainArgs 
    )

    writeToFile(file(baseProject.rootAbsoluteFile, "runcmd.sh"), "#!/bin/bash\n" + cmd.asString)
    logger.info("Running, press ctrl-] to terminate running process...")

    val procHandle = cmd.execAsync()
    @tailrec
    def checkRunning(): TaskResult = {
      if (!procHandle._2.isCompleted) {
        Thread.sleep(1000)
        if (System.in.available > 0 && System.in.read == Task.termChar) {
          logger.info("Terminating: " + className)
          procHandle._1.destroy()
          logger.info("Terminated process for runMain of class : " + className)
          DefaultTaskResult(this, true, sw)
        }
        else checkRunning()
      }
      else {
        Await.result(procHandle._2, Duration.Zero) match {
          case 0 => DefaultTaskResult(this, true, sw)
          case code => DefaultTaskResult(this, false, sw, message = Some("Run Main failed in " + baseProject))
        }
      }
    }
    checkRunning()
  }
}
