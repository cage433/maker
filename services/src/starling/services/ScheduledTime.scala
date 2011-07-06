package starling.services

import java.util.{TimerTask, Timer}
import org.joda.time._
import org.joda.time.format.DateTimeFormat

import starling.calendar.BusinessCalendar
import starling.daterange.Day

import starling.utils.ImplicitConversions._


case class ScheduledTime(description: String, time: DateTime, period: Period, cal: BusinessCalendar) {
  private val periodInSeconds = period.toStandardSeconds.getSeconds.require(_ > 0, "Period cannot be 0")
  def schedule(task: TimerTask, timer: Timer) = timer.scheduleAtFixedRate(task, time.toDate, periodInSeconds * 1000)
  val prettyTime = time.toString(DateTimeFormat.forPattern("HH:mm"))
}

object ScheduledTime {
  def apply(description: String, startingTime: LocalTime, period: Period, cal: BusinessCalendar) =
    new ScheduledTime(description, startingTimeAfterNow(startingTime, period, cal), period, cal)

  def daily(cal: BusinessCalendar, time: LocalTime = 0 H 0) = ScheduledTime("Every day",       time,  Period.days(1),    cal)
  def hourly(cal: BusinessCalendar)                         = ScheduledTime("Every hour",      0 H 0, Period.hours(1),   cal)
  def everyFiveMinutes(cal: BusinessCalendar)               = ScheduledTime("Every 5 minutes", 0 H 0, Period.minutes(5), cal)

  private def startingTimeAfterNow(startingTime: LocalTime, period: Period, cal: BusinessCalendar): DateTime = {
    val scheduledTime = Day.today.atTimeOfDay(startingTime, cal.location)
    val now = cal.location.now
    val periodsToAdd = if (scheduledTime < now) ((now - scheduledTime) / period.toStandardSeconds.getSeconds) + 1 else 0

    scheduledTime + (periodsToAdd * period.toStandardSeconds.getSeconds * 1000)
  }
}