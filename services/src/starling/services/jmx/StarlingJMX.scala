package starling.services.jmx

import management.ManagementFactory
import javax.management.ObjectName
import starling.daterange.Day
import starling.utils.Stopable
import starling.scheduler.{ScheduledTask, Scheduler}

class StarlingJMX(scheduler: Scheduler) extends Stopable {
  val mbs = ManagementFactory.getPlatformMBeanServer

  override def start {
    super.start
    scheduler.getTasks.foreach(task =>
      mbs.registerMBean(new Task(task.task), new ObjectName("Starling.ScheduledTasks:name=" + task.name.replace(',', ';'))))
  }

  trait TaskMBean {
    def activate
    def deactivate
    def isActive: Boolean
    def runNow
    def runNow(observationDay: String)
  }

  class Task(task: ScheduledTask) extends TaskMBean {
    def activate   { task.start }
    def deactivate { task.stop  }
    def isActive   = task.isRunning
    def runNow = task.perform(Day.today)
    def runNow(observationDay: String) = task.perform(Day.parse(observationDay))
  }
}