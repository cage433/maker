package starling.calendar

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.daterange.{Location, Day}

class RegionalHolidaysTest extends TestNGSuite {

  class TestHols extends RegionalHolidays {
    def financialHolidays(name: String) = BusinessCalendarSet("", Location.Unknown, Set())
  }

  @Test
  def testUK {
    val hols = new TestHols().UK

    assertTrue(hols.isHoliday(Day(2011, 4, 29))) // royal wedding
    assertTrue(hols.isHoliday(Day(2011, 5, 30)))
    assertTrue(hols.isHoliday(Day(2012, 6, 5))) // diamond jubilee
    assertTrue(hols.isBusinessDay(Day(2011, 5, 31)))
  }

  @Test
  def testUS {
    val hols = new TestHols().US

    assertTrue(hols.isHoliday(Day(2011, 7, 4))) // royal wedding
    assertTrue(hols.isHoliday(Day(2011, 9, 5)))
    assertTrue(hols.isBusinessDay(Day(2011, 9, 6)))
  }
}