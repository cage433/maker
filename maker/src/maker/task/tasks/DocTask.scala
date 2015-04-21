package maker.task.tasks

import maker.utils.FileUtils._
import java.io.ByteArrayOutputStream
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.Command
import maker.task._
import maker.task.compile._
import maker.project.{BaseProject, Module, Project}
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import maker.ConfigPimps


/** Doc generation task - produces scaladocs from project sources
  *
  * Outputs scala-docs per module in the "docs" sub-dir of the project target output dir
  */
case class DocTask(project : Project) 
  extends Task
  with ConfigPimps
{
  val config = project.config  
  def baseProject = project
  def name = "Doc " + project.name
  def upstreamTasks = project.modules.map(SourceCompileTask(_))
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {

    logger.info("running scala-doc gen for project " + project)

    val inputFiles = project.upstreamModules.flatMap(_.compilePhase.sourceFiles)

    val docDir = project.docOutputDir
    if (inputFiles.nonEmpty && 
        ( !docDir.exists || 
          lastModifiedFileTime(inputFiles).getOrElse(0L) > lastModifiedFileTime(List(docDir)).getOrElse(0L))) {
      logger.debug("generating doc for project " + project.toString)
      if (!docDir.exists) docDir.mkdirs else docDir.deleteAll

      // make a separate opts file as the args can get too big for a single command
      val optsFile = file(docDir, "docopts")
      writeToFile(optsFile, "-classpath " + project.classpathSansScalaLibs(SourceCompilePhase) + " " + inputFiles.mkString(" "))

      val scalaToolsClasspath = config.scalaVersion.scalaJars.mkString(":")

      val bs = new ByteArrayOutputStream()
      val cmd = Command.scalaCommand(
        classpath = scalaToolsClasspath,
        klass = "scala.tools.nsc.ScalaDoc",
        args = Vector("@" + optsFile.getAbsolutePath)
      ).withOutputTo(bs).withWorkingDirectory(docDir)

      cmd.run() match {
        case 0 => DefaultTaskResult(this, true, sw)
        case _ => DefaultTaskResult(this, false, sw, message = Some(bs.toString))
      }
    }
    else {
      logger.debug("not generating doc for project " + project.toString)
      DefaultTaskResult(this, true, sw)
    }
  }
}
