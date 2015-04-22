package maker.task

import maker.utils.Stopwatch
import maker.project.Module
import maker.utils.TableBuilder
import maker.utils.RichString._

trait Task {
  def name : String
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult
  def failureHaltsTaskManager : Boolean = true

  /**
   * Tasks that normally need to run BEFORE this one does 
   */
  def upstreamTasks : Iterable[Task] 
}

object Task {
  val termChar = 29 // ctrl-]

}
