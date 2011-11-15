package starling.scheduler

import org.joda.time._
import org.joda.time.format.DateTimeFormat

import starling.calendar.BusinessCalendar
import starling.daterange.Day

import starling.utils.ImplicitConversions._
import java.util.{TimerTask, Timer}


case class ScheduledTime(description: String, time: LocalTime, period: Period, cal: BusinessCalendar, ignoreMissed: Boolean = true) {
  def schedule(task: TimerTask, timer: Timer) = timer.scheduleAtFixedRate(task, scheduledTime.toDate, periodInSeconds * 1000)
  val prettyTime = scheduledTime.toString(DateTimeFormat.forPattern("HH:mm [dd MMM]"))

  private def scheduledTime: DateTime = {
    val scheduledTime = Day.today.atTimeOfDay(time, cal.location)
    val now = cal.now
    val periodsToAdd = if (scheduledTime < now && ignoreMissed) ((now - scheduledTime) / periodInSeconds) + 1 else 0

    scheduledTime + (periodsToAdd * periodInSeconds * 1000)
  }

  private lazy val periodInSeconds = period.toStandardSeconds.getSeconds.ensuring(_ > 0, "Period cannot be 0")
}

object ScheduledTime {
  def apply(description: String, startingTime: LocalTime, period: Period, cal: BusinessCalendar) =
    new ScheduledTime("Every " + description, startingTime, period, cal)

  def daily(cal: BusinessCalendar, time: LocalTime = 0 H 0)    = ScheduledTime("day",       time,       Period.days(1),    cal)
  def hourly(cal: BusinessCalendar)                            = ScheduledTime("hour",      0 H 0,      Period.hours(1),   cal)
  def everyFiveMinutes(cal: BusinessCalendar, offset: Int = 0) = ScheduledTime("5 minutes", 0 H offset, Period.minutes(5), cal)
}