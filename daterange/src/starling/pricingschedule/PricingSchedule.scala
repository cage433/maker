package starling.pricingschedule

import starling.calendar.BusinessCalendar
import org.apache.commons.lang.NotImplementedException
import starling.daterange.{DateRange, Month, DayOfWeek, Day}

// Used to specify what to count and price
abstract case class PeriodType(period: String) {
  def pricingStartDay(calendar: BusinessCalendar, countingStartDate: Day, numberOfCountingPeriods: Int, countDirection: Direction, countEventAsDay: Int): Day

  def pricingPeriod(calendar: BusinessCalendar, adjustedEventDate: Day, numberOfPricingPeriods: Int,
                    includeEventDate: Boolean, countingStartDate: Day, pricingStartDate: Day,
                    countDirection: Direction): List[Day]
}

object CalendarDayPeriodType extends PeriodType("CalendarDay") {
  def pricingStartDay(calendar: BusinessCalendar, countingStartDate: Day, numberOfCountingPeriods: Int, countDirection: Direction, countEventAsDay: Int) = {
    countDirection match {
      case Before => countingStartDate - (numberOfCountingPeriods - countEventAsDay)
      case After => countingStartDate + (numberOfCountingPeriods - countEventAsDay)
    }
  }

  def pricingPeriod(calendar: BusinessCalendar, adjustedEventDate: Day, numberOfPricingPeriods: Int,
                    includeEventDate: Boolean, countingStartDate: Day, pricingStartDate: Day,
                    countDirection: Direction) = {
    var period = List[Day]()
    var day: Day = pricingStartDate
    while (period.size < numberOfPricingPeriods) {
      if(includeEventDate || day != adjustedEventDate) {
        period ::= day
      }
      day = day + countDirection.value
    }
    period.sorted
  }
}

object WorkingDayPeriodType extends PeriodType("WorkingDay") {
  def pricingStartDay(calendar: BusinessCalendar, countingStartDate: Day, numberOfCountingPeriods: Int, countDirection: Direction, countEventAsDay: Int) = {
    countDirection match {
      case Before => countingStartDate.addBusinessDays(calendar, -(numberOfCountingPeriods - countEventAsDay))
      case After => countingStartDate.addBusinessDays(calendar, numberOfCountingPeriods - countEventAsDay)
    }
  }

  def pricingPeriod(calendar: BusinessCalendar, adjustedEventDate: Day, numberOfPricingPeriods: Int,
                    includeEventDate: Boolean, countingStartDate: Day, pricingStartDate: Day,
                    countDirection: Direction) = {
    var period = List[Day]()
    var day: Day = pricingStartDate
    while (period.size < numberOfPricingPeriods) {
      if (includeEventDate || day != adjustedEventDate) {
        val businessDay = day.isBusinessDay(calendar)
        if (!businessDay && includeEventDate && day == adjustedEventDate && period.nonEmpty) {
          period ::= day
        } else if (businessDay) {
          period ::= day
        }
      }
      day = day + countDirection.value
    }
    period
  }
}

object MonthPeriodType extends PeriodType("Month") {
  def pricingStartDay(calendar: BusinessCalendar, countingStartDate: Day, numberOfCountingPeriods: Int, countDirection: Direction, countEventAsDay: Int) = {
    countDirection match {
      case Before => countingStartDate.addMonths(-(numberOfCountingPeriods - countEventAsDay))
      case After => countingStartDate.addMonths(numberOfCountingPeriods - countEventAsDay)
    }
  }

  def pricingPeriod(calendar: BusinessCalendar, adjustedEventDate: Day, numberOfPricingPeriods: Int,
                    includeEventDate: Boolean, countingStartDate: Day, pricingStartDate: Day,
                    countDirection: Direction) = {
    val (start, end) = countDirection match {
      case Before => {
        val lastMonth = pricingStartDate.containingMonth
        val firstMonth = lastMonth - numberOfPricingPeriods
        (firstMonth, lastMonth)
      }
      case After => {
        val firstMonth = pricingStartDate.containingMonth
        val lastMonth = firstMonth + numberOfPricingPeriods
        (firstMonth, lastMonth)
      }
    }

    var period = List[Day]()
    DateRange(start.firstDay, (end - 1).lastDay).days.toList.map {
      day => {
        if (includeEventDate || day != adjustedEventDate) {
          period ::= day
        }
      }
    }
    period.sorted
  }
}

//object HalfMonthPeriodType extends PeriodType("HalfMonth")
//object DecadePeriodType extends PeriodType("Decade")

object PeriodType {
  val types = List(CalendarDayPeriodType, WorkingDayPeriodType, MonthPeriodType /*, HalfMonthPeriodType, DecadePeriodType*/)

  def parse(s: String) = types.find(_.period.equalsIgnoreCase(s))
}

// Used to specify which day to start counting at
abstract case class CountingStartOption(start: String) {
  def applyRule(calendar: BusinessCalendar, eventDate: Day, weekStarts: DayOfWeek): Day
}


object DayCountingStartOption extends CountingStartOption("Day") {
  def applyRule(calendar: BusinessCalendar, eventDate: Day, weekStarts: DayOfWeek) = eventDate
}

object FirstDayOfWeekCountingStartOption extends CountingStartOption("FirstDayOfWeek") {
  def applyRule(calendar: BusinessCalendar, eventDate: Day, weekStarts: DayOfWeek) = if (eventDate.dayOfWeek.number >= weekStarts.number) {
    eventDate + (weekStarts - eventDate.dayOfWeek) // If we are on or after WeekStart Go back to WeekStart in this Week
  } else {
    eventDate + (weekStarts - eventDate.dayOfWeek) - 7 // Go to WeekStart in previous Week
  }
}

object FirstDayOfMonthCountingStartOption extends CountingStartOption("FirstDayOfMonth") {
  def applyRule(calendar: BusinessCalendar, eventDate: Day, weekStarts: DayOfWeek) = new Month(eventDate.year, eventDate.month).firstDay
}

//object FirstDayOfDecadeCountingStartOption extends CountingStartOption
//object FirstDayOf1stDecadeCountingStartOption extends CountingStartOption
//object FirstDayOf2ndDecadeCountingStartOption extends CountingStartOption
//object FirstDayOf3rdDecadeCountingStartOption extends CountingStartOption
//object FirstDayOfHalfMonthCountingStartOption extends CountingStartOption
//object FirstDayOf1stHalfMonthCountingStartOption extends CountingStartOption
//object FirstDayOf2ndHalfMonthCountingStartOption extends CountingStartOption
object CountingStartOption {
  val types = List(DayCountingStartOption, FirstDayOfWeekCountingStartOption, FirstDayOfMonthCountingStartOption)

  def parse(s: String) = types.find(_.start.equalsIgnoreCase(s))
}

case class PricingPerCalendarPeriodOption(pricing: String)

object NoPricingPeriod extends PricingPerCalendarPeriodOption("None")

object WeekPricingPeriod extends PricingPerCalendarPeriodOption("Week")

object MonthPricingPeriod extends PricingPerCalendarPeriodOption("Month")

object PricingPerCalendarPeriodOption {
  val types = List(NoPricingPeriod, WeekPricingPeriod, MonthPricingPeriod)

  def unapply(a: Any) = types.find(_.pricing.equalsIgnoreCase(a.toString))
}

case class ParcelPricingOption(pricing: String)

object AllParcels extends ParcelPricingOption("AllParcels")

object FirstParcelOnly extends ParcelPricingOption("FirstParcelOnly")

object SecondParcelOnly extends ParcelPricingOption("SecondParcelOnly")

object LastParcelOnly extends ParcelPricingOption("")

object ParcelPricingOption {
  val types = List(AllParcels, FirstParcelOnly, SecondParcelOnly, LastParcelOnly)

  def parse(s: String) = types.find(_.pricing.equalsIgnoreCase(s))
}

case class PricingSchedule(algorithm: PricingScheduleAlgorithm, description: String,
                           eventDateType: String, endEventDateType: String,
                           parcelPricingOption: ParcelPricingOption)
