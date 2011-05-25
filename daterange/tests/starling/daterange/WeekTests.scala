package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations.Test
import scala.util.Random


class WeekTests extends TestNGSuite {
  @Test
  def testWeekDay {
    // from example at http://en.wikipedia.org/wiki/ISO_week_date#Calculating_a_date_given_the_week_number_and_weekday
    val week = new Week(2008, 39)
    assertEquals(Day(2008, 9, 26), week.day(DayOfWeek.friday))
  }

  @Test
  def testYearHundred {
    // years of the form 100 + 400n are a special case, so test these specially for easier debugging. 
    assertTrue(Week.isShortYear(1700))
    assertTrue(Week.isShortYear(2100))
  }

  @Test
  def testIsLongYear {
    // list of long years in a 400-year cycle from http://www.phys.uu.nl/~vgent/calendar/isocalendar.htm
    val longYears = List(
      1604, 1609, 1615, 1620, 1626, 1632, 1637, 1643, 1648, 1654, 1660, 1665, 1671, 1676, 1682, 1688, 1693, 1699,
      1705, 1711, 1716, 1722, 1728, 1733, 1739, 1744, 1750, 1756, 1761, 1767, 1772, 1778, 1784, 1789, 1795,
      1801, 1807, 1812, 1818, 1824, 1829, 1835, 1840, 1846, 1852, 1857, 1863, 1868, 1874, 1880, 1885, 1891, 1896,
      1903, 1908, 1914, 1920, 1925, 1931, 1936, 1942, 1948, 1953, 1959, 1964, 1970, 1976, 1981, 1987, 1992, 1998
    )

    assertEquals((1601 to 1999).filter(!Week.isShortYear(_)).toList, longYears)
  }

  @Test
  def testWeekContains4Jan {
    // the definition of the year/first week is that the first week of the year
    // is always the one containing 4th jan.
    (1981 to 2029).foreach((year) => {
      val fourthJan = Day(year, 1, 4)
      val firstWeek = new Week(year, 1)
      assertEquals(Week.containing(fourthJan), firstWeek)
      assertTrue(firstWeek.contains(fourthJan))
    })
  }

  @Test
  def testYearsMatchUp {
    // test that there are no gaps or overlaps between "years" of weeks.
    val firstYear = 1981
    val lastYear = 2029
    val firstDay = new Week(firstYear, 1).firstDay - 1
    (firstYear to lastYear).foldLeft(firstDay)((prevDay, year) => {
      assertEquals(new Week(year, 1).firstDay, prevDay + 1,
        "week 1 of " + year + " doesn't continue from last week of " + (year-1))
      new Week(year, Week.numWeeksInYear(year)).lastDay
    })
  }

  @Test
  def testFailing {
    // this was a particular case that was failing. it's fixed now, but this is a test
    // worth keeping in case the bug manages to slip back in.
    var week = new Week(2010, 1)
    var day = Day(2010, 1, 1)
    assertTrue(!week.contains(day), "week " + week + " shouldn't contain " + day)
    assertTrue(Week.containing(day) != week, "week containing " + day + " shouldn't be " + week)
  }

  @Test
  def testContaining {
    val range = new SimpleDateRange(Day(2009, 6, 1), Day(2011, 5, 30))
    range.foreach((day) => {
      val week = Week.containing(day)
      assertTrue(week.firstDay <= day, "week " + week + " (" + week.firstDay + "-" + week.lastDay + ") lower bound doesn't contain " + day)
      assertTrue(week.lastDay >= day, "week " + week + " upper bound doesn't contain " + day)
    })
  }

  @Test
  def testArithmetic {
    val random = new Random(123456)

    assertEquals(new Week(2010, 1) + 10, new Week(2010, 11))
    assertEquals(new Week(1981, 1) + 52, new Week(1981, 53))
    assertEquals(new Week(1982, 1) + 52, new Week(1983, 1))

    (0 to 100).foreach((j) => {
      val randomDay = Day(2010, 1, 1) + random.nextInt(365 * 10)
      val week = Week.containing(randomDay)

      (0 to 10).foreach((i) => {
        val advanced = week + i
        assertEquals(advanced - week, i, "" + advanced + " - " + week + " != " + i)
      })
    })
  }

  @Test
  def shouldParseFourDigitYearWithWeekAtTheEnd {
    assertEquals(Week.parse("2009W12"), Week(2009, 12))
    assertEquals(Week.parse("1981w50"), Week(1981, 50))
  }

  @Test
  def shouldParseTwoDigitYearWithWeekAtTheEnd {
    assertEquals(Week.parse("09W12"), Week(2009, 12))
    assertEquals(Week.parse("89W12"), Week(1989, 12))
    assertEquals(Week.parse("09w01"), Week(2009, 1))
  }
}
