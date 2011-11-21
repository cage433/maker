package starling.scheduler

import java.util.{TimerTask, Timer}
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import starling.daterange.{Location, Day}
import starling.utils.{Stopwatch, Enableable, Log}
import org.joda.time.Period


case class TaskDescription(name: String, time: ScheduledTime, task: ScheduledTask, coolDown: Period = Period.ZERO)
  extends TimerTask with Enableable {

  private val log = Log.forClass[Scheduler]
  private val coolDownClock = new Stopwatch().offset(- (coolDown.toStandardSeconds.getSeconds * 1000))
  private def coolDownRemaining = coolDown.toStandardSeconds.getSeconds - coolDownClock.s

  override def enable = task.enable
  override def disable = task.disable
  override def isEnabled = task.isEnabled
  val cal = time.cal
  def attribute(name: String, alternative: String = ""): ScheduledTaskAttribute = task.attribute(name, alternative)

  def schedule(timer: Timer) = log.infoF("%s%s @ %s @ %s (%s @ London), %s" % (isEnabled ? "" | "[DISABLED] ", name,
    time.prettyTime("HH:mm dd MMM"), time.cal.name, time.prettyTime("HH:mm dd MMM", Location.London), time.description)) { time.schedule(this, timer) }

  def run = log.logException("Task %s failed" % name) { skippingReason match {
    case Some(reasonToSkip) => log.info("Skipping '%s', because: %s" % (name, reasonToSkip))
    case None => log.infoWithTime("Executing scheduled task: " + name) { task.perform(Day.today) }; coolDownClock.reset
  } }

  private def skippingReason: Option[String] = if (!isEnabled) Some("Task disabled")
    else if (!Day.today.isBusinessDay(cal)) Some("Not a business day in calendar: " + cal.name)
    else coolDownRemaining |> (remaining => (remaining > 0).option("Task in 'cool down', time remaining: %d seconds" % remaining))
}