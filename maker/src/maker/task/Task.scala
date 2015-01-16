package maker.task

import maker.utils.Stopwatch
import maker.project.BaseProject
import maker.project.Module
import maker.utils.TableBuilder
import maker.utils.RichString._

trait Task {
  def name : String
  def toShortString = toString
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult
  def failureHaltsTaskManager : Boolean = true
  def baseProject : BaseProject

  override def toString = baseProject + " - " + name
  /**
   * Tasks that normally need to run BEFORE this one does 
   */
  def upstreamTasks : Iterable[Task] 
}

abstract class SingleModuleTask(module : Module)
  extends Task
{
  def baseProject = module
}

object Task {
  val termChar = 29 // ctrl-]

}
