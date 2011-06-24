package starling.calendar

import starling.daterange.{Location, DateRange, Day}
import starling.utils.ImplicitConversions._

case class BusinessCalendarSet(name: String, location: Location, holidays: Set[Day]) extends BusinessCalendar {
  def isHoliday(day: Day) = holidays.contains(day)

  def +(other: BusinessCalendarSet):BusinessCalendarSet = other.location match {
    case `location` if other == this => this
    case `location` => BusinessCalendarSet(name + other.name, location, holidays ++ other.holidays)
    case _ => BusinessCalendarSet(name + other.name, Location.Unknown, holidays ++ other.holidays)
  }
}

trait BusinessCalendar { self =>
  def name: String

  def isHoliday(day : Day) : Boolean

  def isBusinessDay(day : Day) = day.isWeekday && !isHoliday(day)

  def nextBusinessDay(day : Day) : Day = thisOrNextBusinessDay(day.nextDay)

  def previousBusinessDay(day : Day) : Day = thisOrPreviousBusinessDay(day.previousDay)
  
  def thisOrPreviousBusinessDay(day : Day) : Day = {
    if(isBusinessDay(day)) day
    else previousBusinessDay(day)
  }

  def thisOrNextBusinessDay(day : Day) : Day = {
    if(isBusinessDay(day)) day
    else nextBusinessDay(day)
  }

  def filter(dateRange: DateRange): List[Day] = dateRange.toList.filter(isBusinessDay(_))

  def location: Location

  def &&(other: BusinessCalendar): BusinessCalendar = new BusinessCalendar {
    def location = Location.Unknown
    def isHoliday(day: Day) = self.isHoliday(day) || other.isHoliday(day)
    def name = "%s && %s" % (self, other)
  }
}

object NilCalendar extends BusinessCalendarSet("None", Location.Unknown, Set())

object BusinessCalendar{
  val NONE = new BusinessCalendar() {
    def name = "<None>"
    def isHoliday(day : Day) = false
    def location = Location.Unknown
  }
  val WeekdayBusinessCalendar = new BusinessCalendar {
    def name = "Weekday"
    def location = Location.Unknown
    def isHoliday(day:Day) = day.isWeekendDay
  }
  def error(code: String) = new BusinessCalendar() {
    def location = Location.Unknown

    def isHoliday(day: Day) = throw new Exception("No holidays for this calendar: " + code)

    def name = {
      "Error no calendar for: " + code
    }
  }
}
