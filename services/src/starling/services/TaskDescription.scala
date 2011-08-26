package starling.services

import java.util.{TimerTask, Timer}
import starling.daterange.Day
import starling.utils.Log

import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._


case class TaskDescription(name: String, time: ScheduledTime, task: ScheduledTask) extends TimerTask {
  val cal = time.cal
  def attribute(name: String, alternative: String = ""): ScheduledTaskAttribute = task.attribute(name, alternative)

  def schedule(timer: Timer) = Log.infoF("Scheduled: %s @ %s @ %s, %s" % (name, time.prettyTime, time.cal.name, time.description)) {
    task.start; time.schedule(this, timer)
  }

  def run = logException("Task %s failed" % name) {
    if (!Day.today.isBusinessDay(cal)) {
      Log.info("Not a business day in calendar: %s, thus skipping: " % (cal.name, name))
    } else if (!task.isRunning) {
      Log.info("Task is stopped, thus skipping: " % (cal.name, name))
    } else Log.infoWithTime("Executing scheduled task: " + name) {
      task.perform(Day.today)
    }
  }
}