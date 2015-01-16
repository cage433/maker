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


/** Doc generation task - produces scaladocs from module sources
  *
  * Outputs scala-docs per module in the "docs" sub-dir of the module target output dir
  */
case class DocTask(module : Module) extends Task {
  
  def name = "Doc " + module.name
  def baseProject = module
  def upstreamTasks = List(SourceCompileTask(module))
  val logger = LoggerFactory.getLogger(this.getClass)
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = module.props

    val runDocLogFile = file("rundoc.out")

    logger.info("running scala-doc gen for module " + module)

    val writer = new PrintWriter(new TeeToFileOutputStream(runDocLogFile))

    val inputFiles = module.compilePhase.sourceFiles

    val docDir = module.docOutputDir
    if (!docDir.exists || lastModifiedFileTime(inputFiles).getOrElse(0L) > lastModifiedFileTime(List(docDir)).getOrElse(0L)) {
      logger.debug("generating doc for module " + module.toString)
      if (!docDir.exists) docDir.mkdirs else docDir.deleteAll

      // make a separate opts file as the args can get too big for a single command
      val optsFile = file(docDir, "docopts")
      writeToFile(optsFile, "-classpath " + module.compilePhase.compilationClasspath + " " + inputFiles.mkString(" "))

      val scalaToolsClasspath = module.props.ProjectScalaCompilerJar().getAbsolutePath + ":" + module.props.ProjectScalaLibraryJar().getAbsolutePath + ":" + module.props.ProjectScalaReflectJar().getAbsolutePath

      val cmd = ScalaDocCmd(
        new CommandOutputHandler(Some(writer)).withSavedOutput,
        docDir,
        props.Java().getAbsolutePath,
        scalaToolsClasspath,
        Nil,
        optsFile)

      println(cmd)
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
