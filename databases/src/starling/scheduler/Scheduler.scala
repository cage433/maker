package starling.scheduler

import starling.props.Props
import starling.utils.{Log, Stopable}

import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer
import java.util.concurrent.ScheduledExecutorService


class Scheduler(props: Props, scheduleExecutorService: ScheduledExecutorService, initialTasks: List[TaskDescription] = Nil)
  extends Stopable with Log {

  def getTasks: List[TaskDescription] = tasks.toList

  private val tasks = new ListBuffer[TaskDescription]() ++= initialTasks

  override def start = log.infoF("%s tasks for ServerType: %s" %(tasks.size, props.ServerType())) {
    super.start;
    tasks.map(_.schedule(scheduleExecutorService))
  }

  override def stop = {
    super.stop; scheduleExecutorService.shutdownNow()
  }

  def +=(task: TaskDescription) = {
    tasks += task

    if (isRunning) task.schedule(scheduleExecutorService)
  }
}
