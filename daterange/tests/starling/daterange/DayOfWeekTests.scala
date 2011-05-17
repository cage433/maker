package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.daterange.Day._


class DayOfWeekTests extends TestNGSuite {
  @Test
  def testParse {
    assertEquals(DayOfWeek.parse("Monday"), DayOfWeek.monday)
    assertEquals(DayOfWeek.parse("tuesday"), DayOfWeek.tuesday)
    assertEquals(DayOfWeek.parse("WED"), DayOfWeek.wednesday)
    assertEquals(DayOfWeek.parse("fri"), DayOfWeek.friday)
    assertEquals(DayOfWeek.parse("sUN"), DayOfWeek.sunday)
  }
}
