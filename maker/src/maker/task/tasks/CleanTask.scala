package maker.task.tasks

import maker.project.{Module, ProjectTrait, Project}
import maker.utils.FileUtils._
import maker.task._
import maker.utils.Stopwatch
import org.slf4j.LoggerFactory
import maker.task.compile.{SourceCompilePhase, TestCompilePhase}


/** Clean task - cleans up all build artifacts from the classpath
  *
  *  removes all build content and directories that contained it
  */
case class CleanTask(project : ProjectTrait, majorScalaVersion : String) extends Task
{
  def name = "Clean"
  lazy val logger = LoggerFactory.getLogger(this.getClass)
  def upstreamTasks = Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    logger.debug("cleaning " + project)

    // remove all output as we don't want lingering files or even empty dirs messing up a subsequent builds

               
    project.upstreamModules.foreach{
      module => 
        cleanRegularFilesLeavingDirectories(module.classDirectory(majorScalaVersion, SourceCompilePhase))
        cleanRegularFilesLeavingDirectories(module.classDirectory(majorScalaVersion, TestCompilePhase))
        cleanRegularFilesLeavingDirectories(module.managedLibDir(majorScalaVersion))
        cleanRegularFilesLeavingDirectories(module.managedResourceDir)
        cleanRegularFilesLeavingDirectories(module.managedLibSourceDir(majorScalaVersion))
        cleanRegularFilesLeavingDirectories(module.testManagedLibDir(majorScalaVersion))
        cleanRegularFilesLeavingDirectories(module.testManagedLibSourceDir(majorScalaVersion))
        recursiveDelete(module.compilePhase.phaseDirectory)
        recursiveDelete(module.testCompilePhase.phaseDirectory)
        module.compilePhase.compilationCacheFile.delete
        module.testCompilePhase.compilationCacheFile.delete
    }

    project match {
      case p : Project => 
        recursiveDelete(p.packageDir)
        cleanRegularFilesLeavingDirectories(p.managedLibDir(majorScalaVersion))
        cleanRegularFilesLeavingDirectories(p.managedResourceDir)
        cleanRegularFilesLeavingDirectories(p.managedLibSourceDir(majorScalaVersion))
        cleanRegularFilesLeavingDirectories(p.testManagedLibDir(majorScalaVersion))
        cleanRegularFilesLeavingDirectories(p.testManagedLibSourceDir(majorScalaVersion))
      case _ =>
    }

    DefaultTaskResult(this, succeeded = true, stopwatch = sw)
  }
}
