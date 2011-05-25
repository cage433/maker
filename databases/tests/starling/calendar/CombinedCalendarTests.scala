package starling.calendar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import org.scalatest.prop.Checkers
import org.scalacheck.{Prop, Gen, Arbitrary}
import starling.daterange.Day._
import starling.daterange.{Location, Day}

import Prop._


class CombinedCalendarTests extends WordSpec with Checkers with ShouldMatchers {
  val everydayAHoliday = new BusinessCalendar {
    def isHoliday(day: Day) = true
    def location = Location.Unknown
    def name = "every day a holiday"
    override def toString = name
  }

  val everydayABusinessDay = new BusinessCalendar {
    def isHoliday(day: Day) = false
    def location = Location.Unknown
    def name = "every day a business day"
    override def toString = name
  }

  implicit def arbitraryFixing: Arbitrary[BusinessCalendar] = Arbitrary {
    Gen.oneOf(everydayAHoliday, everydayABusinessDay)
  }

  implicit def arbitraryDay: Arbitrary[Day] = Arbitrary {
    for (julianDay <- Gen.choose((01 Jan 2008).julianDayNumber, (31 Dec 2011).julianDayNumber))
      yield Day.fromJulianDayNumber(julianDay)
  }

  "Should be a holiday if it's a holiday in either underlying calendar" in {
    check((left: BusinessCalendar, right: BusinessCalendar, day: Day) =>
      left.isHoliday(day) || right.isHoliday(day) ==> (left && right).isHoliday(day)
    )
  }
}