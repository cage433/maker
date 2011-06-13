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
  // During development it's necessary to speed up time to force scheduled tasks to run earlier
  private val speedup = 1//600

  private val OneDay = scale(Period.days(1))
  private val OneHour = scale(Period.hours(1))
  private val Midnight = scale(new LocalTime(0, 0), new LocalTime)

  def daily(cal: BusinessCalendar, time: LocalTime = 0 H 0) =
    new ScheduledTime("Every day", usingCalendar(cal, OneDay, scale(time)), OneDay, cal)

  def hourly(cal: BusinessCalendar) = new ScheduledTime("Every hour", usingCalendar(cal, OneHour, Midnight), OneHour, cal)

  private def usingCalendar(cal: BusinessCalendar, period: Period, time: LocalTime): DateTime = {
    val now = cal.location.now

    val scheduledTime = Day.today.atTimeOfDay(time, cal.location)

    if (scheduledTime < now) {
      val periodsToAdd = ((now - scheduledTime) / period.toStandardSeconds.getSeconds) + 1
      scheduledTime + (periodsToAdd * period.toStandardSeconds.getSeconds * 1000)
    } else {
      scheduledTime
    }
  }

  private def scale[T](normal: => T, scaled: => T) = if (speedup == 1) normal else scaled
  private def scale(period: Period): Period = scale(period, Period.seconds(period.toStandardSeconds.getSeconds / speedup))

  private def scale(time: LocalTime): LocalTime =
    scale(time, LocalTime.fromMillisOfDay(time.getMillisOfDay / speedup + Midnight.getMillisOfDay, time.getChronology))
}