package maker.task.tasks

import maker.project.{BaseProject, Module, Project}
import maker.task.compile.SourceCompilePhase
import maker.utils.Stopwatch
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils.FileUtils._
import scala.collection.immutable.Nil
import maker.utils.os.Command

case class BundleTask(baseProject : BaseProject, version : String, signArtifacts : Boolean = true) extends Task{
  def name = s"Bundle $baseProject"
  def module = baseProject

  def upstreamTasks = PublishLocalTask(baseProject, baseProject.allUpstreamModules, version, signArtifacts) :: Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    import baseProject.{bundleJar, publishLocalPomDir, publishLocalJarDir}
    if (bundleJar.exists)
      bundleJar.delete

    val maybeError = Vector(publishLocalPomDir(version), publishLocalJarDir(version)).foldLeft(None : Option[String]){
      case (Some(error), _) => Some(error)
      case (None, dir) => 
        val cmd = Command(
          baseProject.props.Jar().getAbsolutePath,
          if (bundleJar.exists) "uf" else "cf",
          bundleJar.getAbsolutePath,
          "-C",
          dir.getAbsolutePath,
          "."
        ).withNoOutput
        if (cmd.exec == 0)
          None
        else
          Some(cmd.savedOutput)
    }
    maybeError match {
      case None => 
        DefaultTaskResult(this, true, sw)
      case error  => 
        DefaultTaskResult(this, false, sw, message = error)
    }
  }
}
