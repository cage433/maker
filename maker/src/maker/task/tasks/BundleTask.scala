package maker.task.tasks

import maker.project.{BaseProject, Module, Project}
import maker.task.compile.SourceCompilePhase
import maker.utils.Stopwatch
import maker.task.{Task, TaskResult}
import maker.utils.FileUtils._

case class BundleTask(baseProject : BaseProject, version : String) extends Task{
  def name = "bundle"
  def baseProjects = Vector(baseProject)
  def module = baseProject

  def upstreamTasks = PublishLocalTask(baseProject, baseProject.allUpstreamModules, version, signArtifacts = true) :: Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val bundleJar = file(baseProject.rootAbsoluteFile, "bundle.jar")
    if (bundleJar.exists)
      bundleJar.delete
    null
  }
}
