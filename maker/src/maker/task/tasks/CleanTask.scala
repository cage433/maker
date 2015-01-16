package maker.task.tasks

import maker.project.Module
import maker.utils.FileUtils._
import maker.task._
import maker.utils.Stopwatch
import org.slf4j.LoggerFactory
import maker.task.compile.{SourceCompilePhase, TestCompilePhase}


/** Clean task - cleans up all build artifacts from the classpath
  *
  *  removes all build content and directories that contained it
  */
case class CleanTask(module : Module, deleteManagedLibs : Boolean = false) 
  extends SingleModuleTask(module)
{
  def name = "Clean"
  val logger = LoggerFactory.getLogger(this.getClass)
  def upstreamTasks = (module.immediateUpstreamModules ::: module.immediateUpstreamTestModules).distinct.map(CleanTask(_, deleteManagedLibs))

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = module.props
    logger.debug("cleaning " + module)
    if (deleteManagedLibs){
      Option(module.managedLibDir.listFiles).foreach(_.foreach(_.delete))
    }

    // remove all output as we don't want lingering files or even empty dirs messing up a subsequent builds
    module.packageJar(SourceCompilePhase).delete
    module.packageJar(TestCompilePhase).delete

    cleanRegularFilesLeavingDirectories(module.compilePhase.outputDir)
    cleanRegularFilesLeavingDirectories(module.testCompilePhase.outputDir)
    cleanRegularFilesLeavingDirectories(module.packageDir)
    cleanRegularFilesLeavingDirectories(module.docOutputDir)
    cleanRegularFilesLeavingDirectories(module.targetDir)
    recursiveDelete(module.compilePhase.phaseDirectory)
    recursiveDelete(module.testCompilePhase.phaseDirectory)
    module.compilePhase.compilationCacheFile.delete
    module.testCompilePhase.compilationCacheFile.delete

    DefaultTaskResult(this, succeeded = true, stopwatch = sw)
  }
}
