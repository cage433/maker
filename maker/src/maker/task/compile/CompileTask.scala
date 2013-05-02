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

abstract class CompileTask extends Task{
  
  def project : Project
  def phase : CompilePhase

  val projectPhase = ProjectPhase(project, phase)

  private val log = project.log

  private def copyResourcesToTargetDirIfNecessary(){
    if (props.CopyResourcesBeforeCompiling()) {
      project.layout.resourceDirs.foreach(rd =>
        Option(rd).foreach(d =>
          if (d.exists) copyDirectoryToDirectory(d, project.layout.targetDir)
        )
      )
    }
  }

  private val classesCache:Option[PersistentCache] = project.props.CompilationCache() match {
    case ""|"None"|"No"|"Off" => None
    case "file" => Some(new FileSysyemPersistentCache(project.cacheDirectory))
    case hostname => Some(RedisPersistentCache.instance(hostname))
  }
  def inputsHash = HashCache.hash(projectPhase.compilationDependencies())

  private def cachedCompilation(sw : Stopwatch) : Option[TaskResult] = {
    classesCache collect {
      case cache if cache.contains(inputsHash) ⇒ {
        val files = CompilationCache.lookup(cache, inputsHash).get
        files.copyTo(projectPhase.project.rootAbsoluteFile, projectPhase.outputDir, projectPhase.makerDirectory)
        success(this, sw).withInfo(CompilationInfo(this, CachedCompilation))
      }
    }
  }

  def compilationRequired(upstreamTaskResults : List[TaskResult]) = {
    def hasDeletedSourceFiles = projectPhase.sourceFilesDeletedSinceLastCompilation.nonEmpty
    def upstreamCompilation = upstreamTaskResults.flatMap(_.compilationInfo).exists(_.state != CompilationNotRequired)
    def modificationSinceLastCompilation = (projectPhase.lastSourceModifcationTime, projectPhase.lastCompilationTime) match {
      case (Some(t1), Some(t2)) ⇒ t1 > t2
      case _ ⇒ true
    }
    hasDeletedSourceFiles ||  upstreamCompilation || modificationSinceLastCompilation
  }


  def exec(upstreamTaskResults : List[TaskResult], sw : Stopwatch) : TaskResult = {

    if (projectPhase.sourceFiles.isEmpty){
      cleanRegularFilesLeavingDirectories(projectPhase.outputDir)
      projectPhase.compilationCacheFile.delete
      return success(this, sw).withInfo(CompilationInfo(this, CompilationNotRequired))
    }
    copyResourcesToTargetDirIfNecessary()
    val result = cachedCompilation(sw).getOrElse{
      if (compilationRequired(upstreamTaskResults)){
        
        if (props.UseZincCompiler()){
          val exitCode = ZincCompile(projectPhase) 
            if (exitCode == 0){
            success(this, sw).withInfo(CompilationInfo(this, CompilationSucceeded))
          } else {
            failure(this, sw, "compilation failure")
          }
        } else {

          CompileScalaTask(projectPhase).exec match {
            case Left(e) ⇒ {
              failure(this, sw, "compilation failure").withInfo(CompilationFailedInfo(e))
            }
            case Right(a) ⇒ {
              success(this, sw).withInfo(CompilationInfo(this, CompilationSucceeded))
            }
          }
        }
      } else {
        success(this, sw).withInfo(CompilationInfo(this, CompilationNotRequired))
      }
    }
    classesCache.foreach{
      cache ⇒  CompilationCache.save(cache, project.rootAbsoluteFile, inputsHash, projectPhase.outputDir, projectPhase.makerDirectory)
    }
    result
  }


  def name = phase.toString 
  override def numberOfSourceFiles = projectPhase.sourceFiles.size



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


case class SourceCompileTask(project :Project) extends CompileTask{
  def upstreamTasks = {
    UpdateTask(project, omitIfNoIvyChanges = true) :: upstreamProjects.map(SourceCompileTask)
  }
  def phase = SourceCompilePhase
  override def toShortString = project + ":SC"
}

case class TestCompileTask(project : Project) extends CompileTask{
  def upstreamTasks = {
    var tasks : List[Task] = SourceCompileTask(project) :: upstreamTestProjects.map(TestCompileTask)
    tasks
  }

  def phase = TestCompilePhase
  override def toShortString = project + ":TC"
}

object CompileTask{
  def apply(project : Project, phase : CompilePhase) : CompileTask = {
    phase match{
      case SourceCompilePhase ⇒ SourceCompileTask(project)
      case TestCompilePhase ⇒ TestCompileTask(project)
    }
  }
}
