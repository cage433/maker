package maker.task.tasks

import maker.project.{Module, ProjectTrait, Project}
import maker.utils.FileUtils._
import maker.task._
import maker.utils.Stopwatch
import org.slf4j.LoggerFactory
import maker.task.compile.{SourceCompilePhase, TestCompilePhase}
import maker.ScalaVersion


/** Clean task - cleans up all build artifacts from the classpath
  *
  *  removes all build content and directories that contained it
  */
case class CleanTask(project : ProjectTrait, scalaVersion : ScalaVersion) extends Task
{
  def name = "Clean"
  lazy val logger = LoggerFactory.getLogger(this.getClass)
  def upstreamTasks = Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    logger.debug("cleaning " + project)

    // remove all output as we don't want lingering files or even empty dirs messing up a subsequent builds

               
    project.upstreamModules.foreach{
      module => 
        cleanRegularFilesLeavingDirectories(module.classDirectory(scalaVersion, SourceCompilePhase))
        cleanRegularFilesLeavingDirectories(module.classDirectory(scalaVersion, TestCompilePhase))
        cleanRegularFilesLeavingDirectories(module.managedLibDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(module.managedResourceDir)
        cleanRegularFilesLeavingDirectories(module.managedLibSourceDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(module.testManagedLibDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(module.testManagedLibSourceDir(scalaVersion))
        recursiveDelete(module.compilationMetadataDirectory(scalaVersion, SourceCompilePhase))
        recursiveDelete(module.compilationMetadataDirectory(scalaVersion, TestCompilePhase))
    }

    project match {
      case p : Project => 
        recursiveDelete(p.packageDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(p.managedLibDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(p.managedResourceDir)
        cleanRegularFilesLeavingDirectories(p.managedLibSourceDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(p.testManagedLibDir(scalaVersion))
        cleanRegularFilesLeavingDirectories(p.testManagedLibSourceDir(scalaVersion))
      case _ =>
    }

    DefaultTaskResult(this, succeeded = true, stopwatch = sw)
  }
}
