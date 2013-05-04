package maker.task.tasks

import maker.task.Task
import maker.project.Project
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.utils.os.Command
import maker.utils.FileUtils._
import maker.utils.Utils._
import java.io.File
import maker.task.compile.SourceCompileTask
import maker.task.compile._

case class PackageJarTask(project : Project, aggregateDependentModules : Boolean = false, includeDependentLibs : Boolean = false) extends Task {
  def name = "Package Jar"
  val log = props.log
  val layout = project.layout

  def upstreamTasks = SourceCompileTask(project) :: (if (aggregateDependentModules) Nil else upstreamProjects.map(PackageJarTask(_, aggregateDependentModules, includeDependentLibs)))

  // Note: until we support scopes properly we have to be careful what we put on the runtime classpath
  //   and in package runtime binary artifacts (so test scope content is deliberately excluded here)
  private lazy val dirsToPack : List[File] =  (if (aggregateDependentModules) project.allUpstreamProjects else List(project)).flatMap {
    p =>
        p.layout.resourceDirs + p.compilePhase.outputDir
  }.filter(_.exists)

  def exec(results : List[TaskResult], sw : Stopwatch) = {
    synchronized{
      if (!(aggregateDependentModules || includeDependentLibs) && fileIsLaterThan(project.outputArtifact, dirsToPack)) {
        log.info("Packaging up to date for " + project.name + ", skipping...")
        TaskResult.success(this, sw)
      } else {
        doPackage(results, sw)
      }
    }
  }

  private def doPackage(results : List[TaskResult], sw : Stopwatch) = {
    val jar = props.Jar().getAbsolutePath

    case class WrappedCommand(cmd : Command, ignoreFailure : Boolean){
      def exec : Int = {
        (cmd.exec, ignoreFailure) match {
          case  (0, _) ⇒ 0
          case (errorNo, true) ⇒ {
            log.warn("Ignoring  error in " + cmd + ". Artifact may be missing content")
            0
          }
          case (errorNo, _) ⇒ errorNo
        }
      }
    }
    def jarCommand(updateOrCreate : String, dir : File) = WrappedCommand( 
      Command(props, List(jar, "cf", project.outputArtifact.getAbsolutePath, "-C", dir.getAbsolutePath, "."): _*),
      ignoreFailure = false
    )
    def createJarCommand(dir : File) = jarCommand("cf", dir)
    def updateJarCommand(dir : File) = jarCommand("uf", dir)

    if (!layout.packageDir.exists)
      layout.packageDir.mkdirs

    // Note: until we support scopes properly we have to be careful what we put on the runtime classpath
    //   and in package runtime binary artifacts (so test scope content is deliberately excluded here)
    val dirsToPack : List[File] =  (if (aggregateDependentModules) project.allUpstreamProjects else List(project)).flatMap {
      p =>
          p.layout.resourceDirs + p.compilePhase.outputDir
    }.filter(_.exists)

    var cmds : List[WrappedCommand] = createJarCommand(dirsToPack.head) :: dirsToPack.tail.map(updateJarCommand)
    if (includeDependentLibs){
      val tmpPackagingDir = file(project.layout.packageDir, "tmp").asNewDirectory.asAbsoluteFile
      def unpackJarCommand(j : File) = WrappedCommand(
        Command(props, Some(tmpPackagingDir), "unzip", "-o", j.getAbsolutePath, "-d", tmpPackagingDir.getAbsolutePath), 
        ignoreFailure = true
      )
      cmds = cmds ::: project.classpathJars.toList.map(unpackJarCommand)
      cmds = cmds ::: List(updateJarCommand(tmpPackagingDir))
    }

    cmds.find(_.exec != 0) match {
      case Some(failingCommand) ⇒ TaskResult.failure(this, sw, failingCommand.cmd.savedOutput)
      case None ⇒ TaskResult.success(this, sw)
    }
  }
}
