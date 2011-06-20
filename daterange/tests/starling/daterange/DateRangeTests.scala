package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.utils.ScalaTestUtils._
import collection.immutable.{SortedSet, TreeSet}
import java.lang.String

class DateRangeTests extends TestNGSuite {

  /**
   * Converting a month to a string and back again should yield
   * the original month
   */
  @Test
  def testMonthParsing {
    for (m <- 1 to 12) {
      assertEquals(Month.parse(Month(2009, m).toPrintableForm), Month(2009, m))
    }
  }

  @DataProvider(name = "testParse2Provider")
  def testParse2Provider = {
    constructArgs(
      ("Aug - 09", Month(2009, 8)),
      ("SEPTEMBER 2010", Month(2010, 9))
      )
  }


  /**
   * Test parsing for various month strings
   */
  @Test(dataProvider = "testParse2Provider")
  def testMonthParsing2(text: String, month: Month) {
    assertEquals(Month.parse(text), month)
  }


  /**
   * Test conversion to a julian day number and back again
   */
  @Test
  def testJulianDayNumber {
    var baseJdn = Day.julianDayNumber(Day(2009, 8, 21))
    for (i <- 0 to 1000) {
      val jdn = baseJdn + i
      val d = Day.fromJulianDayNumber(jdn)
      val jdn2 = Day.julianDayNumber(d)
      assertEquals(jdn2, jdn)

    }
  }

  @Test
  def testOrderingOfDayTimes {
    val monday = Day(2009, 9, 14)
    val tuesday = monday + 1
    val wednesday = tuesday + 1

    assertTrue(monday.endOfDay < tuesday.startOfDay)
    assertTrue(monday.endOfDay <= tuesday.startOfDay)
    assertTrue(tuesday.startOfDay > monday.startOfDay)

    assertTrue(monday.startOfDay < monday.endOfDay)
    assertEquals(monday.startOfDay.compare(monday.startOfDay), 0)

    assertTrue(monday.startOfDay < wednesday.startOfDay)
    assertTrue(wednesday.startOfDay >= tuesday.endOfDay)
  }

  @Test
  def testMaxDay {
    val d1 = Day(2011, 1, 19)
    val d2 = Day(2011, 1, 20)
    val listOfDays = List(d1,d2)
    val maxDay = listOfDays.max
    assertEquals(maxDay, d2)
  }

  @Test
  def testAddTenorToDay {
    val d1 = Day(2010, 1, 13)
    assertEquals(d1.add(1, Month), Day(2010, 2, 13))
    val d2 = Day(2010, 1, 31)
    assertEquals(d2.add(1, Month), Day(2010, 2, 28)) // goes to last day in month
    val d3 = Day(2008, 2, 29)
    assertEquals(d3.add(1, Year), Day(2009, 2, 28)) // goes to last day in month
    val d4 = Day(2012, 1, 29)
    assertEquals(d4.add(1, Month), Day(2012, 2, 29)) // leap year
  }

  @Test
  def testDateRangesAreIterable {
    val m = Month(2009, 9)
    val dayList = m.iterator.toList
    assertEquals(dayList.size, 30)
    assertEquals(dayList, dayList.sortWith(_ < _))
    assertEquals(Day(2009, 9, 1), dayList.head)
    assertEquals(Day(2009, 9, 30), dayList.last)
  }

  @Test
  def testDayArithmetic {
    val monday = Day(2009, 9, 21)
    val tuesday = Day(2009, 9, 22)
    val wednesday = Day(2009, 9, 23)
    val nextMonday = Day(2009, 9, 28)

    assertEquals(monday + 1, tuesday)
    assertEquals(wednesday - 2, monday)
    assertEquals(wednesday.dayOnOrAfter(DayOfWeek.monday), nextMonday)
    assertEquals(2, wednesday - monday)
    assertEquals(monday.containingMonth, Month(2009, 9))
  }

  @Test
  def testMonthArithmetic {
    val nov = Month(2009, 11)
    val dec = Month(2009, 12)
    val jan = Month(2010, 1)
    assertEquals(nov.next, dec)
    assertEquals(dec.next, jan)
    assertEquals(jan.previous, dec)
  }

  @Test
  def testDayParsing {
    try {
      val parsedDay = Day.parse("112Dec09")
      assertTrue(false, "date shouldn't have parsed")
    }
    catch {
      case _ : DayParseException => assertTrue(true)
    }
    assertEquals(Day.parse("11Dec09"), Day(2009, 12, 11))
    assertEquals(Day.parse("11Dec09Z"), Day(2009, 12, 11))
    assertEquals(Day.parse("2007-08-10Z"), Day(2007, 8, 10))
    assertEquals(Day.parse("2007-08-10"), Day(2007, 8, 10))
    assertEquals(Day.parse("11Dec2009"), Day(2009, 12, 11))
    assertEquals(Day.parse("31Oct2019"), Day(2019, 10, 31))
    assertEquals(Day.parse("1Feb2009"), Day(2009, 2, 1))
  }

  /**
   * regression test for isWednesday bug
   */
  @Test
  def isWednesday {
    val wed = Day(2009, 12, 2)
    assertTrue(wed.isWednesday, "Failed the Wednesday test")
    val thu = Day(2009, 12, 3)
    assertFalse(thu.isWednesday, "Failed the Wednesday test")
  }

  /**
   * check date range behaviour in sorted container. this is so that we're certain
   * the Ordered[T] conversion hack is working properly.
   */
  @Test
  def testOrderingOnDateRange {
    val year = new Year(2010)
    val month_jan = new Month(2010, 1)
    val month_feb = new Month(2010, 2)
    val day_first_jan = Day(2010, 1, 1)
    val day_last_jan = Day(2010, 1, 31)
    val day_first_feb = Day(2010, 2, 1)

    val set : SortedSet[DateRange] = TreeSet[DateRange](List(
        day_first_feb, day_first_jan, year, month_feb, day_last_jan, month_jan
      ): _ *)
    assertEquals(set.toList, List(
      year, month_jan, day_first_jan, day_last_jan, month_feb, day_first_feb
    ))
  }

  /**
   * check that ordering still works on derived types too.
   */
  @Test
  def testOrderingOnDay {
    val month = new Month(2010, 1)
    val set : SortedSet[Day] = TreeSet.empty[Day](Day.ordering) ++ month.toList

    // this assumes the implementation of Month.toList returns the days in-order, which
    // is currently the case.
    assertEquals(set.toList, month.toList)
  }

  @Test
  def testBreakIntoMonths{
    assertEquals(List(Month(2009, 1)), Month(2009, 1).toListOfMonths)
    assertEquals(List(Month(2009, 1), Month(2009, 2), Month(2009, 3)), Quarter(2009, 1).toListOfMonths)
    val yearMonths = Year(2009).toListOfMonths
    assertEquals(Month(2009, 1), yearMonths.head)
    assertEquals(Month(2009, 12), yearMonths.last)
    assertEquals(yearMonths, yearMonths.sortWith(_<_))
  Day}

  @Test
  def testContainingQuarter{
    for (iQuarter <- 1 to 4){
      val qtr = Quarter(2010, iQuarter)
      for (d <- qtr){
        assertEquals(qtr, d.containingQuarter)
      }
    }
  }

  @DataProvider(name = "testJodaRoundTripProvider")
  def testJodaRoundTripProvider = constructDataProviderArgs(List(
    Day(2009, 1, 1), Day(2010, 10, 10), Day(1990, 8, 13)
  ))
  @Test(dataProvider = "testJodaRoundTripProvider")
  def testJodaRoundTrip(day : Day){
    assertEquals(day, Day.fromJodaDate(day.toJodaLocalDate))
  }
}


