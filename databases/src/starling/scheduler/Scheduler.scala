package starling.scheduler

import java.util.Timer

import starling.props.Props
import starling.utils.{Log, Stopable}

import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer


class Scheduler(props: Props, initialTasks: List[TaskDescription] = Nil) extends Stopable with Log {
  def getTasks: List[TaskDescription] = tasks.toList

  private val tasks = new ListBuffer[TaskDescription]() ++= initialTasks
  private lazy val timer = new Timer(true)

  override def start = log.infoF("%s tasks for ServerType: %s" %(tasks.size, props.ServerType())) {
    super.start;
    tasks.map(_.schedule(timer))
  }

  override def stop = {
    super.stop; timer.cancel
  }

  def +=(task: TaskDescription) = {
    tasks += task

    if (isRunning) task.schedule(timer)
  }
}
