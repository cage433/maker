package starling.scheduler

import java.util.{TimerTask, Timer}
import starling.daterange.Day
import starling.utils.ImplicitConversions._
import org.joda.time.Period
import starling.utils.{Enableable, Log}
import scalaz.Scalaz._


case class TaskDescription(name: String, time: ScheduledTime, task: ScheduledTask) extends TimerTask with Enableable {
  override def enable = task.enable
  override def disable = task.disable
  override def isEnabled = task.isEnabled
  val log = Log.forClass[Scheduler]
  val cal = time.cal
  def attribute(name: String, alternative: String = ""): ScheduledTaskAttribute = task.attribute(name, alternative)

  def schedule(timer: Timer) = log.infoF("%s%s @ %s @ %s, %s" %
    (isEnabled ? "" | "[DISABLED] ", name, time.prettyTime, time.cal.name, time.description)) { time.schedule(this, timer) }

  def run = log.logException("Task %s failed" % name) {
    if (!Day.today.isBusinessDay(cal)) {
      log.info("Not a business day in calendar: %s, thus skipping: %s" % (cal.name, name))
    } else if (!isEnabled) {
      log.info("Skipping disabled 'scheduled' task: " + name)
    } else log.infoWithTime("Executing scheduled task: " + name) {
      task.perform(Day.today)
    }
  }
}