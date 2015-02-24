package maker.task.tasks

import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.{ScalaDocCmd, CommandOutputHandler}
import maker.task._
import maker.task.compile._
import maker.project.{BaseProject, Module}
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import maker.MakerConfig


/** Doc generation task - produces scaladocs from module sources
  *
  * Outputs scala-docs per module in the "docs" sub-dir of the module target output dir
  */
case class DocTask(module : Module) 
  extends Task
  with MakerConfig
{
    
  def baseProject = module
  def name = "Doc " + module.name
  def upstreamTasks = List(SourceCompileTask(module))
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {

    logger.info("running scala-doc gen for module " + module)

    val inputFiles = module.compilePhase.sourceFiles

    val docDir = module.docOutputDir
    if (inputFiles.nonEmpty && 
        ( !docDir.exists || 
          lastModifiedFileTime(inputFiles).getOrElse(0L) > lastModifiedFileTime(List(docDir)).getOrElse(0L))) {
      logger.debug("generating doc for module " + module.toString)
      if (!docDir.exists) docDir.mkdirs else docDir.deleteAll

      // make a separate opts file as the args can get too big for a single command
      val optsFile = file(docDir, "docopts")
      writeToFile(optsFile, "-classpath " + module.compilePhase.compilationClasspath + " " + inputFiles.mkString(" "))

      val scalaToolsClasspath = config.scalaVersion.resources.map(_.resourceFile.getAbsolutePath).mkString(":")

      val cmd = ScalaDocCmd(
        CommandOutputHandler.NULL.withSavedOutput,
        docDir,
        config.javaExecutable.getAbsolutePath,
        scalaToolsClasspath,
        Nil,
        optsFile)

      cmd.exec() match {
        case 0 => DefaultTaskResult(this, true, sw)
        case _ => DefaultTaskResult(this, false, sw, message = Some(cmd.savedOutput))
      }
    }
    else {
      logger.debug("not generating doc for module " + module.toString)
      DefaultTaskResult(this, true, sw)
    }
  }
}
