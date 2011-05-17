package starling.calendar


import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import org.mockito.Mockito._
import starling.daterange.{Location, Day}

class BusinessCalendarTest extends StarlingTest {

  val ukHols = BusinessCalendarSet("UK", Location.Unknown, Set(Day(2010, 1, 1),Day(2010, 1, 2),Day(2009, 12, 25),Day(2009, 12, 26),Day(2010, 12, 27),Day(2010, 12, 25),Day(2010, 12, 28),Day(2011, 1, 3)))

  @Test
  def testUK {
    val holidaysTable = mock(classOf[HolidayTables])
    when(holidaysTable.UK) thenReturn ukHols 
    val calendars = new BusinessCalendars(holidaysTable)
    
    assertTrue(calendars.UK.isBusinessDay(Day(2010, 1, 4)))
    assertTrue(calendars.UK.isBusinessDay(Day(2009, 12, 31)))

    assertFalse(calendars.UK.isBusinessDay(Day(2010, 1, 1)))
    assertFalse(calendars.UK.isBusinessDay(Day(2010, 1, 2)))
    assertFalse(calendars.UK.isBusinessDay(Day(2009, 12, 26)))

    assertTrue(calendars.UK.isHoliday(Day(2010, 12, 27)))
    assertTrue(calendars.UK.isHoliday(Day(2009, 12, 25)))
    assertTrue(calendars.UK.isHoliday(Day(2010, 12, 28)))
    
    assertEquals(calendars.UK.nextBusinessDay(Day(2010, 12, 24)), Day(2010, 12, 29))
    assertEquals(calendars.UK.nextBusinessDay(Day(2010, 12, 23)), Day(2010, 12, 24))
    assertEquals(calendars.UK.nextBusinessDay(Day(2010, 12, 31)), Day(2011, 1, 4))

    assertEquals(calendars.UK.previousBusinessDay(Day(2010, 12, 29)), Day(2010, 12, 24))
    
    assertEquals(calendars.UK.thisOrNextBusinessDay(Day(2010, 12, 25)), Day(2010, 12, 29))
    assertEquals(calendars.UK.thisOrNextBusinessDay(Day(2010, 12, 24)), Day(2010, 12, 24))
  }

  @Test
  def testBusinessDaysBetween {
    val holidaysTable = mock(classOf[HolidayTables])
    when(holidaysTable.UK) thenReturn ukHols
    val calendars = new BusinessCalendars(holidaysTable)

    val day1 = Day(2010,8,11)
    val day2 = Day(2010,8,11)
    assertEquals(0, day1.businessDaysBetween(day2, calendars.UK))

    val monday = Day(2010,8,9)
    val previousFriday = Day(2010,8,6)
    assertEquals(-1, monday.businessDaysBetween(previousFriday, calendars.UK))
    assertEquals(1, previousFriday.businessDaysBetween(monday, calendars.UK))

    val tuesdayAfterHolidayMonday = Day(2011,1,4)
    val pFriday = Day(2010,12,31)
    assertEquals(-1, tuesdayAfterHolidayMonday.businessDaysBetween(pFriday, calendars.UK))

    val thursday = Day(2010,8,12)
    val otherMonday = Day(2010,8,9)
    assertEquals(-3, thursday.businessDaysBetween(otherMonday, calendars.UK))
  }

  @Test
  def testBusinessDaysBetweenNonBusinessDays {
    val holidaysTable = mock(classOf[HolidayTables])
    when(holidaysTable.UK) thenReturn ukHols
    val calendars = new BusinessCalendars(holidaysTable)
    val holiday = Day(2010,12,27)
    val otherDay = Day(2010,12,24)
    intercept[IllegalArgumentException] {
      otherDay.businessDaysBetween(holiday, calendars.UK)
    }
  }
}
