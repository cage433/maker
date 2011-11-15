package starling.daterange

import starling.calendar.BusinessCalendar

/** A day together with a time of day.
 */
case class DayAndTime(day : Day, timeOfDay0 : TimeOfDay) extends Ordered[DayAndTime] {
	import TimeOfDay._

  def timeOfDay = timeOfDay0

  def compare(rhs : DayAndTime) : Int = {
    if (day < rhs.day)
      -2
    else if (day > rhs.day)
      2
    else (timeOfDay, rhs.timeOfDay) match {
      case (StartOfDay, EndOfDay) => -1
      case (EndOfDay, StartOfDay) => 1
      case _ => 0
    }
  }

  def isEndOfDay = timeOfDay == TimeOfDay.EndOfDay

  def nextDay = copy(day = day.nextDay)
  def nextBusinessDay(calendar : BusinessCalendar) = copy(day = day.nextBusinessDay(calendar))
  def previousBusinessDay(calendar : BusinessCalendar) = copy(day = day.previousBusinessDay(calendar))
  def containingMonth = day.containingMonth
  def + (n : Int) : DayAndTime = copy(day = day + n)
  def - (n : Int) : DayAndTime = this + (-n)

  def copyTimeOfDay(newTimeOfDay : TimeOfDay) = copy(timeOfDay0 = newTimeOfDay)

  def timeSince(that : DayAndTime) : Double = {
    if (that > this)
      -that.timeSince(this)
    else
      (day - that.day + timeOfDay.daysSince(that.timeOfDay)) / Day.daysInYear
  }
}

class DayAndNoTime(day: Day) extends DayAndTime(day, TimeOfDay.EndOfDay) {
  override def timeOfDay = throw new Exception("Time of day should not be accessed")
}
