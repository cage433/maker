package starling.services

import java.util.{TimerTask, Timer}
import starling.daterange.Day
import starling.utils.Log

import starling.utils.ImplicitConversions._


case class TaskDescription(name: String, time: ScheduledTime, task: ScheduledTask) extends TimerTask {
  val log = Log.forClass[Scheduler]
  val cal = time.cal
  def attribute(name: String, alternative: String = ""): ScheduledTaskAttribute = task.attribute(name, alternative)

  def schedule(timer: Timer) = log.infoF("%s @ %s @ %s, %s" % (name, time.prettyTime, time.cal.name, time.description)) {
    task.start; time.schedule(this, timer)
  }

  def run = log.logException("Task %s failed" % name) {
    if (!Day.today.isBusinessDay(cal)) {
      log.info("Not a business day in calendar: %s, thus skipping: " % (cal.name, name))
    } else if (!task.isRunning) {
      log.info("Task is stopped, thus skipping: " % (cal.name, name))
    } else log.infoWithTime("Executing scheduled task: " + name) {
      task.perform(Day.today)
    }
  }
}