package maker.task.compile

import maker.project._
import maker.task.Task
import maker.task.TaskResult
import maker.utils.FileUtils._
import maker.utils.Stopwatch
import org.apache.commons.io.FileUtils._
import maker.task.TaskResult._
import maker.MakerProps
import maker.task.tasks.UpdateTask
import maker.utils.PersistentCache
import maker.utils.FileSysyemPersistentCache
import maker.utils.RedisPersistentCache
import maker.utils.HashCache
import maker.utils.CompilationCache
import maker.task.Build
import sbt.compiler.CompileFailed
import maker.utils.TaskInfo
import java.util.Date

abstract class CompileTask extends Task{
  
  def module : Module
  def phase : CompilePhase
  val props = module.props

  val modulePhase = ModuleCompilePhase(module, phase)

  private val log = module.log

  private def copyResourcesToTargetDirIfNecessary(){
    if (props.CopyResourcesBeforeCompiling()) {
      val d = module.resourceDir
      if (d.exists) copyDirectoryToDirectory(d, module.targetDir)
    }
  }

  private val classesCache:Option[PersistentCache] = module.props.CompilationCache() match {
    case ""|"None"|"No"|"Off" => None
    case "file" => Some(new FileSysyemPersistentCache(module.cacheDirectory))
    case hostname => Some(RedisPersistentCache.instance(hostname))
  }
  def inputsHash = HashCache.hash(modulePhase.compilationDependencies())

  private def cachedCompilation(sw : Stopwatch) : Option[TaskResult] = {
    classesCache collect {
      case cache if cache.contains(inputsHash) => {
        val files = CompilationCache.lookup(cache, inputsHash).get
        files.copyTo(modulePhase.module.rootAbsoluteFile, modulePhase.outputDir, modulePhase.phaseDirectory)
        success(this, sw).withInfo(CompilationInfo(this, CachedCompilation))
      }
    }
  }

  def compilationRequired(upstreamTaskResults : Iterable[TaskResult]) = {
    def hasDeletedSourceFiles = modulePhase.sourceFilesDeletedSinceLastCompilation.nonEmpty
    def upstreamCompilation = upstreamTaskResults.flatMap(_.compilationInfo).exists(_.state != CompilationNotRequired)
    def modificationSinceLastCompilation = (modulePhase.lastSourceModifcationTime, modulePhase.lastCompilationTime) match {
      case (Some(t1), Some(t2)) => t1 > t2
      case _ => true
    }
    hasDeletedSourceFiles ||  upstreamCompilation || modificationSinceLastCompilation
  }


  def exec(upstreamTaskResults : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    if (modulePhase.sourceFiles.isEmpty){
      cleanRegularFilesLeavingDirectories(modulePhase.outputDir)
      modulePhase.compilationCacheFile.delete
      return success(this, sw).withInfo(CompilationInfo(this, CompilationNotRequired))
    }
    copyResourcesToTargetDirIfNecessary()
    val result = cachedCompilation(sw).getOrElse{
      if (compilationRequired(upstreamTaskResults)){
        props.Compiler() match {
          case "zinc" => 
            val exitCode = ZincCompile(modulePhase)
            if (exitCode == 0)
              success(this, sw).withInfo(CompilationInfo(this, CompilationSucceeded))
            else 
              failure(this, sw, "compilation failure")
          case "scalac" => 
            CompileScalaTask(modulePhase).exec match {
              case Left(e) => {
                failure(this, sw, "compilation failure").withInfo(CompilationFailedInfo(e))
              }
              case Right(a) => {
                success(this, sw).withInfo(CompilationInfo(this, CompilationSucceeded))
              }
            }
          case "dummy-test-compiler" =>
            DummyCompileTask(modulePhase).exec
            success(this, sw)

        }
      } else {
        success(this, sw).withInfo(CompilationInfo(this, CompilationNotRequired))
      }
    }
    classesCache.foreach{
      cache =>  CompilationCache.save(cache, module.rootAbsoluteFile, inputsHash, modulePhase.outputDir, modulePhase.phaseDirectory)
    }
    result
  }


  def name = phase.toString 

}

case class CompilationFailedInfo(e : CompileFailed) extends TaskInfo{
  def failingFiles = e.problems.toList.map(_.position.sourceFile).filter(_.isDefined).map(_.get).distinct
  private def toString_(files : List[String]) = {
    val b = new StringBuffer
    b.append("Compilation failed for\n")
    b.append(files.mkString("\n"))
    b.toString
  }
  def toShortString = {
    toString_(failingFiles.map(_.getName))
  }
  def toLongString = {
    toString_(failingFiles.map(_.getPath))
  }
}


case class SourceCompileTask(module :Module) extends CompileTask{
  def upstreamTasks = {
    module.immediateUpstreamModules.map(SourceCompileTask) ++ List(UpdateTask(module))
  }
  def phase = SourceCompilePhase
  override def toShortString = module + ":SC"
}

case class TestCompileTask(module : Module) extends CompileTask{
  def upstreamTasks = {
    var tasks : List[Task] = SourceCompileTask(module) :: module.immediateUpstreamTestModules.map(TestCompileTask)
    tasks
  }

  def phase = TestCompilePhase
  override def toShortString = module + ":TC"
}

object CompileTask{
  def apply(module : Module, phase : CompilePhase) : CompileTask = {
    phase match{
      case SourceCompilePhase => SourceCompileTask(module)
      case TestCompilePhase => TestCompileTask(module)
    }
  }
}
