package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations.Test

class YearTests extends TestNGSuite {
  @Test
  def shouldBeContigousWithPreviousAndFollowingYears {
    (1981 to 2028).foreach((yNum : Int) => {
      val year = Year(yNum)

      assertEquals(year.previous.lastDay.nextDay, year.firstDay)
      assertEquals(year.next.firstDay.previousDay, year.lastDay)
    })
  }

  @Test
  def shouldBeUsableInArithmetic {
    assertEquals((Year(2010) + 1) - Year(2010), 1)
  }

  @Test
  def shouldGiveNewYearsDayAsFirstDay {
    assertEquals(Year(2010).firstDay, Day(2010, 1, 1))
  }

  @Test
  def shouldGive31DecAsLastDay {
    assertEquals(Year(2010).lastDay, Day(2010, 12, 31))
  }

  @Test
  def shouldParseAFourDigitYear {
    assertEquals(Year.parse("2000"), Year(2000))
    assertEquals(Year.parse("1901"), Year(1901))
    assertEquals(Year.parse("2100"), Year(2100))
  }

  @Test
  def shouldParseATwoDigitYearAsNearMillennium {
    assertEquals(Year.parse("09"), Year(2009))
    assertEquals(Year.parse("14"), Year(2014))
    assertEquals(Year.parse("80"), Year(1980))
    assertEquals(Year.parse("79"), Year(2079))
    assertEquals(Year.parse("95"), Year(1995))
  }

  @Test
  def shouldParseAYearWithCalAtTheBeginning {
    assertEquals(Year.parse("CAL09"), Year(2009))
    assertEquals(Year.parse("Cal99"), Year(1999))
    assertEquals(Year.parse("cal14"), Year(2014))
  }

  @Test
  def shouldParseAYearBeginningWithY {
    assertEquals(Year.parse("Y09"), Year(2009))
    assertEquals(Year.parse("y99"), Year(1999))
    assertEquals(Year.parse("y14"), Year(2014))
  }
}
