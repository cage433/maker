package maker.task.tasks

import maker.task.Task
import maker.project.Project
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.utils.os.Command
import maker.utils.FileUtils._
import maker.utils.Utils._
import java.io.File
import maker.project.WebProject
import maker.project.WebAppDetails
import org.apache.commons.io.FileUtils._
import maker.utils.os.CommandOutputHandler
import maker.task.compile.SourceCompileTask
import maker.task.compile._

case class PackageWarTask(project : Project with WebProject) extends Task{

  def name = "Package War"
  val log = props.log
  val layout = project.layout

  def upstreamTasks = SourceCompileTask(project) :: Nil

  def exec(results : List[TaskResult], sw : Stopwatch) = {
    synchronized{
      doPackage(results, sw)
    }
  }

  private def doPackage(results : List[TaskResult], sw : Stopwatch) = {

    val jar = props.Jar().getAbsolutePath

    // Note: until we support scopes properly we have to be careful what we put on the runtime classpath
    //   and in package runtime binary artifacts (so test scope content is deliberately excluded here)
    val dirsToPack = layout.resourceDirs + project.compilePhase.outputDir

    val WebAppDetails(webAppDir, port) = project.webAppDetails
    // basic structure
    // WEB-INF
    // WEB-INF/lib
    // WEB-INF/classes
    // META-INF/
    log.info("Packaging web app, web app dir = " + webAppDir.getAbsolutePath)

    // build up the war structure image so we can make a web archive from it...
    val warImage = file(project.layout.packageDir, "webapp")

    log.info("Making war image..." + warImage.getAbsolutePath)
    if (warImage.exists) recursiveDelete(warImage)
    warImage.mkdirs()

    // copy war content to image
    copyDirectory(webAppDir, warImage)
    dirsToPack.foreach(dir => copyDirectory(dir, file(warImage, "WEB-INF/classes")))

    // until we properly support scopes, treat lib provided as runtime provided scope and so exclude it from war packaging
    val allLibs = project.classpathJarsOnly.filter(_.exists) //.flatMap(_.listFiles)
    log.debug("allLibs: ")
    allLibs.foreach(f => log.debug(f.getAbsolutePath))
    val providedLibJars : Set[File] = project.layout.providedLibDirs.filter(_.exists).flatMap(f => Option(f.listFiles).map(_.toList)).flatten
    log.debug("classpath providedLibs libs:")
    providedLibJars.foreach(f => log.debug(f.getAbsolutePath))
    log.debug("additional named provided libs:")
    val providedLibNames = project.dependencyAdjustments.providedLibNames
    providedLibNames.foreach(log.debug(_))
    val allProvidedLibNames = providedLibNames ++ providedLibJars.map(_.getName)
    val allLibsButNotProvidedLibs = allLibs.filter(f => !allProvidedLibNames.exists(libName => f.getName.contains(libName)))
    log.debug("allLibsButNotProvidedLibs:")
    allLibsButNotProvidedLibs.foreach(f => {
      log.debug(f.getAbsolutePath)
      copyFileToDirectory(f, file(warImage, "WEB-INF/lib"))
    })

    log.info("Packaging artifact " + project.outputArtifact.getAbsolutePath)
    val cmd = Command(props, CommandOutputHandler().withSavedOutput, None, List(jar, "cf", project.outputArtifact.getAbsolutePath, "-C", warImage.getAbsolutePath, "."): _*)
    if (cmd.exec == 0)
      TaskResult.success(this, sw)
    else 
      TaskResult.failure(this, sw, cmd.savedOutput)

  }

}
