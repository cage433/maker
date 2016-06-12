package maker.task.tasks

import maker.project.{Module, ProjectTrait, Project}
import maker.utils.FileUtils._
import maker.task._
import maker.utils.Stopwatch
import maker.task.compile.{SourceCompilePhase, TestCompilePhase}
import maker.{ScalaVersion, Log}


/** Clean task - cleans up all build artifacts from the classpath
  *
  *  removes all build content and directories that contained it
  */
case class CleanTask(project : ProjectTrait) extends Task with Log
{
  def name = "Clean"
  def upstreamTasks = project match {
    case m: Module => 
      m.upstreamModules.filterNot(_ == m).map(CleanTask(_))
    case p: ProjectTrait =>
      p.upstreamModules.map(CleanTask(_))
  }

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    project match {
      case p : Project =>
        recursiveDelete(p.packageDir)
        cleanRegularFilesLeavingDirectories(p.managedResourceDir)
        p.clearDependencies()

      case module: Module => 
        logger.debug("cleaning " + module)

        cleanRegularFilesLeavingDirectories(module.classDirectory(SourceCompilePhase))
        cleanRegularFilesLeavingDirectories(module.classDirectory(TestCompilePhase))
        cleanRegularFilesLeavingDirectories(module.managedResourceDir)
        recursiveDelete(module.compilationMetadataDirectory(SourceCompilePhase))
        recursiveDelete(module.compilationMetadataDirectory(TestCompilePhase))
        module.clearDependencies()
    }

    DefaultTaskResult(this, succeeded = true, stopwatch = sw)
  }
}
