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
case class CleanTask(project : ProjectTrait) extends Task
{
  def name = "Clean"
  lazy val logger = LoggerFactory.getLogger(this.getClass)
  def upstreamTasks = Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    logger.debug("cleaning " + project)

    // remove all output as we don't want lingering files or even empty dirs messing up a subsequent builds

    project.upstreamModules.foreach{
      module => 
        cleanRegularFilesLeavingDirectories(module.compilePhase.outputDir)
        cleanRegularFilesLeavingDirectories(module.testCompilePhase.outputDir)
        cleanRegularFilesLeavingDirectories(module.targetDir)
        recursiveDelete(module.compilePhase.phaseDirectory)
        recursiveDelete(module.testCompilePhase.phaseDirectory)
        module.compilePhase.compilationCacheFile.delete
        module.testCompilePhase.compilationCacheFile.delete
    }

    project match {
      case p : Project => 
        recursiveDelete(p.packageDir)
      case _ =>
    }

    DefaultTaskResult(this, succeeded = true, stopwatch = sw)
  }
}
