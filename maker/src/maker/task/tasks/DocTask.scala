package maker.task.tasks

import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.{TeeToFileOutputStream, Stopwatch}
import maker.utils.os.{ScalaDocCmd, CommandOutputHandler}
import maker.task._
import maker.task.compile._
import maker.project.BaseProject
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory


/** Doc generation task - produces scaladocs from module sources
  *
  * Outputs scala-docs per module in the "docs" sub-dir of the module target output dir
  */
case class DocTask(baseProject : BaseProject) extends Task {
  
  def name = "Doc " + baseProject.name
  def upstreamTasks = baseProject.allUpstreamModules.map(SourceCompileTask).toList
  def module = baseProject
  val logger = LoggerFactory.getLogger(this.getClass)
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = baseProject.props

    val runDocLogFile = file("rundoc.out")

    logger.info("running scala-doc gen for module " + baseProject)

    val writer = new PrintWriter(new TeeToFileOutputStream(runDocLogFile))

    val projects = baseProject.allUpstreamModules
    val (classpath, inputFiles) = (
      projects.map(_.compilePhase.compilationClasspath).mkString(":"),
      projects.flatMap(_.compilePhase.sourceFiles))


    val docDir = baseProject.docOutputDir
    if (!docDir.exists || lastModifiedFileTime(inputFiles).getOrElse(0L) > lastModifiedFileTime(List(docDir)).getOrElse(0L)) {
      logger.debug("generating doc for module " + baseProject.toString)
      if (!docDir.exists) docDir.mkdirs else docDir.deleteAll

      // make a separate opts file as the args can get too big for a single command
      val optsFile = file(docDir, "docopts")
      writeToFile(optsFile, "-classpath " + classpath + " " + inputFiles.mkString(" "))

      val scalaToolsClasspath = baseProject.props.ProjectScalaCompilerJar().getAbsolutePath + ":" + baseProject.props.ProjectScalaLibraryJar().getAbsolutePath

      val cmd = ScalaDocCmd(
        new CommandOutputHandler(Some(writer)).withSavedOutput,
        docDir,
        props.Java().getAbsolutePath,
        scalaToolsClasspath,
        Nil,
        optsFile)

      cmd.exec() match {
        case 0 => DefaultTaskResult(this, true, sw)
        case _ => DefaultTaskResult(this, false, sw, message = Some(cmd.savedOutput))
      }
    }
    else {
      logger.debug("not generating doc for module " + baseProject.toString)
      DefaultTaskResult(this, true, sw)
    }
  }
}
