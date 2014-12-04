package maker.task.compile

import maker.project._
import maker.task.Task
import maker.task.TaskResult
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.utils._
import org.apache.commons.io.FileUtils._
import maker.task.TaskResult._
import sbt.compiler.CompileFailed
import maker.task.tasks.UpdateTask
import maker.task.SingleModuleTask
import xsbti.Problem
import xsbti.Severity

abstract class CompileTask extends Task{
  
  def module : Module
  def baseProject = module
  def phase : CompilePhase
  val props = module.props

  val modulePhase = ModuleCompilePhase(module, phase)

  private def copyResourcesToTargetDirIfNecessary(){
    if (props.CopyResourcesBeforeCompiling()) {
      val d = module.resourceDir
      if (d.exists) copyDirectoryToDirectory(d, module.targetDir)
    }
  }

  private val classesCache:Option[PersistentCache] = module.props.CompilationCache() match {
    case ""|"None"|"No"|"Off" => None
    case "file" => Some(new FileSystemPersistentCache(module.cacheDirectory))
    case hostname => Some(RedisPersistentCache.instance(hostname))
  }
  def inputsHash = HashCache.hash(modulePhase.compilationDependencies())

  private def successfulResult(sw : Stopwatch, state : CompilationState) = CompileTaskResult(
    this, succeeded = true, 
    stopwatch = sw, 
    state = state
  )
  private def cachedCompilation(sw : Stopwatch) : Option[TaskResult] = {
    classesCache collect {
      case cache if cache.contains(inputsHash) => {
        val files = CompilationCache.lookup(cache, inputsHash).get
        files.copyTo(modulePhase.module.rootAbsoluteFile, modulePhase.outputDir, modulePhase.phaseDirectory)
        successfulResult(sw, CachedCompilation)
      }
    }
  }

  def compilationRequired(upstreamTaskResults : Iterable[TaskResult]) = {
    def hasDeletedSourceFiles = modulePhase.sourceFilesDeletedSinceLastCompilation.nonEmpty

    def upstreamCompilation = upstreamTaskResults.exists{
      case r : CompileTaskResult if r.state != CompilationNotRequired => true
      case _ => false
    }

    def modificationSinceLastCompilation = (modulePhase.lastSourceModifcationTime, modulePhase.lastCompilationTime) match {
      case (Some(t1), Some(t2)) => t1 > t2
      case _ => true
    }
    def lastCompilationFailed = modulePhase.lastCompilationFailed()
    hasDeletedSourceFiles ||  upstreamCompilation || modificationSinceLastCompilation || lastCompilationFailed
  }


  def exec(upstreamTaskResults : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    if (modulePhase.sourceFiles.isEmpty){
      cleanRegularFilesLeavingDirectories(modulePhase.outputDir)
      modulePhase.compilationCacheFile.delete
      return successfulResult(sw, CompilationNotRequired)
    }
    copyResourcesToTargetDirIfNecessary()
    val result = cachedCompilation(sw).getOrElse{
      if (compilationRequired(upstreamTaskResults)){
        props.Compiler() match {
          case "zinc" => 
            val exitCode = ZincCompile(modulePhase)
            if (exitCode == 0)
              successfulResult(sw, CompilationSucceeded)
            else {
              modulePhase.markCompilatonFailure()
              CompileTaskResult(
                this, succeeded = false,
                stopwatch = sw, state = CompilationFailed("compilation failure")
              )
            }
          case "dummy-test-compiler" =>
            DummyCompileTask(modulePhase).exec
            successfulResult(sw, CompilationSucceeded)

        }
      } else {
        successfulResult(sw, CompilationNotRequired)
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
    module.immediateUpstreamModules.map(SourceCompileTask) ++ List(UpdateTask(module, forceSourceUpdate = false))
  }
  def phase = SourceCompilePhase
  override def toShortString = module + ":SC"
}

case class TestCompileTask(module : Module) extends CompileTask{
  def upstreamTasks: List[Task] =
      SourceCompileTask(module) :: module.immediateUpstreamTestModules.map(TestCompileTask)

  def phase = TestCompilePhase
  override def toShortString = module + ":TC"
}

object CompileTask{
  def CALL_TO_COMPILER = "CALL TO SCALA COMPILER"
  def apply(module : Module, phase : CompilePhase) : CompileTask = {
    phase match{
      case SourceCompilePhase => SourceCompileTask(module)
      case TestCompilePhase => TestCompileTask(module)
    }
  }

  def reportOnCompilationErrors(taskResults : List[TaskResult]){
    def position(prob : Problem) = {
      val p = prob.position
      var text = ""
      if (! p.sourceFile.isDefined)
        ("Unknown", "Unknown")
      else if (! p.line.isDefined)
        (p.sourceFile.get.basename, "Unknown")
      else
        (p.sourceFile.get.basename, p.line.get.toString)
    }

    val failures : List[(Module, String, String, String)] = taskResults.collect{
      case CompileTaskResult(
        task : CompileTask,
        false,
        _,
        _,
        Some(compFailed),
        _,
        _
      ) =>
        val severeProblems = compFailed.problems.filter(_.severity == Severity.Error)
        severeProblems.map{
          prob =>
            val (sourceFile, lineNo) = position(prob)
            (task.baseProject, prob.message, sourceFile, lineNo)
        }
    }.toList.flatten

    if (failures.nonEmpty){
      val tb = TableBuilder(
        "Module       ",
        "Message                              ", 
        "Line  ",
        "File                    ")
      failures.foreach{
        case (module, message, sourceFile, lineNo) => 
            tb.addRow(module, message, lineNo, sourceFile)
      }
      println("Compiler errors".inBlue)
      println(tb)
    }
  }
}

case class CompileTaskResult(
  task : CompileTask, 
  succeeded : Boolean, 
  stopwatch : Stopwatch,
  state : CompilationState,
  maybeCompileFailed : Option[CompileFailed] = None,
  message : Option[String] = None, 
  exception : Option[Throwable] = None
) extends TaskResult
