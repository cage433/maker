package starling.services.jmx

import management.ManagementFactory
import javax.management.ObjectName
import starling.daterange.Day
import starling.scheduler.{ScheduledTask, Scheduler}
import starling.utils.{Enableable, Stopable}

class StarlingJMX(scheduler: Scheduler) extends Stopable {
  val mbs = ManagementFactory.getPlatformMBeanServer

  override def start {
    super.start
    scheduler.getTasks.foreach(task =>
      mbs.registerMBean(new Task(task.task), new ObjectName("Starling.ScheduledTasks:name=" + task.name.replace(',', ';'))))
  }

  trait TaskMBean {
    def enable
    def disable
    def isEnabled: Boolean
    def runNow
    def runNow(observationDay: String)
  }

  class Task(task: ScheduledTask) extends TaskMBean with Enableable {
    override def enable  = task.enable
    override def disable = task.disable
    override def isEnabled  = task.isEnabled
    def runNow = task.perform(Day.today)
    def runNow(observationDay: String) = task.perform(Day.parse(observationDay))
  }
}