package maker.task

import maker.utils.Stopwatch
import maker.project.BaseProject
import maker.project.Module
import maker.utils.TableBuilder
import maker.utils.RichString._

trait Task {
  def name : String
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult
  def failureHaltsTaskManager : Boolean = true

  protected def baseProjects : Seq[BaseProject] 

  override def toString = name
  /**
   * Tasks that normally need to run BEFORE this one does 
   */
  def upstreamTasks : Iterable[Task] 
  def extraUpstreamTasks = baseProjects.flatMap(_.extraUpstreamTasks(this))
  def extraDownstreamTasks = baseProjects.flatMap(_.extraDownstreamTasks(this))
}

abstract class SingleModuleTask(module : Module)
  extends Task
{
  def baseProjects = Vector(module)
  override def toString = module + " - " + name
}

object Task {
  val termChar = 29 // ctrl-]

}
