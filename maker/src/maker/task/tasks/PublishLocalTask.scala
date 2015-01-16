package maker.task.tasks

import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils.{Stopwatch, FileUtils}
import maker.utils.maven.IvyLock
import maker.PomUtils
import maker.task.compile.SourceCompilePhase

/**
 * publishes poms and packaged artifacts to the local filesystem 
 * Optionally can include upstream modules, in case it's more
 * convenient to deploy a project as a single jar
 */
case class PublishLocalTask(baseProject : BaseProject, version : String) extends Task {
  def name = "Publish Local"

  def module = baseProject
  def upstreamTasks = baseProject match {
    case _ : Project => baseProject.immediateUpstreamModules.map(PublishLocalTask(_, version))
    case m : Module => PackageJarTask(m, SourceCompilePhase, includeUpstreamModules = false) :: 
      baseProject.immediateUpstreamModules.map(PublishLocalTask(_, version))
  }

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(baseProject, results, sw)
    }
  }
  
  private def doPublish(baseProject: BaseProject, results : Iterable[TaskResult], sw : Stopwatch) = {

    FileUtils.writeToFile(baseProject.publishLocalPomFile, PomUtils.pomXml(baseProject, version))

    baseProject match {
      case _ : Project => 
      case m : Module =>
        copyFileToDirectory(m.packageJar(SourceCompilePhase), m.publishLocalJarDir)
    }
    DefaultTaskResult(this, true, sw)
  }
}
