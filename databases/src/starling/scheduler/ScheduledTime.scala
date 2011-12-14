package starling.scheduler

import org.joda.time._
import org.joda.time.format.DateTimeFormat

import starling.calendar.BusinessCalendar
import starling.utils.ImplicitConversions._
import starling.daterange.{Timestamp, Location, Day}
import java.util.concurrent.{TimeUnit, ScheduledExecutorService}


case class ScheduledTime(description: String, time: LocalTime, period: Period, cal: BusinessCalendar, ignoreMissed: Boolean = true) {
  def schedule(task: Runnable, scheduleExecutorService: ScheduledExecutorService) {
    scheduleExecutorService.scheduleAtFixedRate(task, initialDelayInSeconds, periodInSeconds, TimeUnit.SECONDS)
  }

  val prettyTime: String = prettyTime("HH:mm")

  def prettyTime(format: String, location: Location = cal.location): String =
    scheduledTime(location).toString(DateTimeFormat.forPattern(format))

  def scheduledTime(location: Location): DateTime = scheduledTime.toDateTime(location.timeZoneOn(Day.today))

  def scheduledTime: DateTime = {
    val scheduledTime = Day.today.atTimeOfDay(time, cal.location)
    val now = cal.now
    val periodsToAdd = if (scheduledTime < now && ignoreMissed) ((now - scheduledTime) / periodInSeconds) + 1 else 0

    scheduledTime + (periodsToAdd * periodInSeconds * 1000)
  }

  def initialDelayInSeconds: Long = (scheduledTime.toDate.getTime - Timestamp.now.instant) / 1000

  private lazy val periodInSeconds = period.toStandardSeconds.getSeconds.ensuring(_ > 0, "Period cannot be 0")
}

object ScheduledTime {
  def apply(description: String, startingTime: LocalTime, period: Period, cal: BusinessCalendar) =
    new ScheduledTime("Every " + description, startingTime, period, cal)

  def daily(cal: BusinessCalendar, time: LocalTime = 0 H 0)    = ScheduledTime("day",       time,       Period.days(1),    cal)
  def hourly(cal: BusinessCalendar)                            = ScheduledTime("hour",      0 H 0,      Period.hours(1),   cal)
  def everyFiveMinutes(cal: BusinessCalendar, offset: Int = 0) = ScheduledTime("5 minutes", 0 H offset, Period.minutes(5), cal)
}