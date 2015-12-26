package maker.task.tasks

import maker.utils.FileUtils._
import java.io.ByteArrayOutputStream
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.Command
import maker.task._
import maker.task.compile._
import maker.project.{Module, Project}
import maker.{ScalaVersion, Log}


/** Doc generation task - produces scaladocs from project sources
  *
  * Outputs scala-docs per module in the "docs" sub-dir of the project target output dir
  */
case class DocTask(project : Project) 
  extends Task with Log
{
  def name = "Doc " + project.name
  def upstreamTasks = project.modules.map(CompileTask(project, _, SourceCompilePhase))
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {

    logger.info("running scala-doc gen for project " + project)

    val inputFiles = project.upstreamModules.flatMap(_.sourceFiles(SourceCompilePhase))

    val docDir = project.docOutputDir
    if (inputFiles.nonEmpty && 
        ( !docDir.exists || 
          lastModifiedFileTime(inputFiles).getOrElse(0L) > lastModifiedFileTime(List(docDir)).getOrElse(0L))) {
      logger.debug("generating doc for project " + project.toString)
      if (!docDir.exists) docDir.mkdirs else docDir.deleteAll

      // make a separate opts file as the args can get too big for a single command
      val optsFile = file(docDir, "docopts")
      val classpath = Module.asClasspathStr(
        project.upstreamModules.map(_.classDirectory(SourceCompilePhase))
      )
      writeToFile(optsFile, s"""-classpath $classpath ${inputFiles.mkString(" ")}""")

      val scalaToolsClasspath = project.scalaJars().map(_.getAbsolutePath).mkString(":")

      val bs = new ByteArrayOutputStream()
      val cmd = Command.scalaCommand(
        project,
        classpath = scalaToolsClasspath,
        klass = "scala.tools.nsc.ScalaDoc",
        args = Vector("@" + optsFile.getAbsolutePath)
      ).withOutputTo(bs).withWorkingDirectory(docDir)

      println(bs.toString)
      cmd.run() match {
        case 0 => DefaultTaskResult(this, true, sw)
        case _ => 
          DefaultTaskResult(this, false, sw, message = Some(bs.toString))
      }
    }
    else {
      logger.debug("not generating doc for project " + project.toString)
      DefaultTaskResult(this, true, sw)
    }
  }
}
