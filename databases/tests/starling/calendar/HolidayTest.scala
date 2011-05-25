package starling.calendar

import starling.utils.StarlingTest
import starling.daterange.Day
import org.testng.annotations.Test
import org.testng.Assert._

class HolidayTest extends StarlingTest {

  @Test
  def easter {
    assertTrue(Holiday.isEasterMonday(Day(2009,4,13)))
    assertTrue(Holiday.isGoodFriday(Day(2009,4,10)))
    assertTrue(Holiday.isEarlyMayBankHoliday(Day(2009,5,4)))
    assertFalse(Holiday.isEarlyMayBankHoliday(Day(2009,5,5)))
    assertFalse(Holiday.isEarlyMayBankHoliday(Day(2009,5,11)))
    assertTrue(Holiday.isSpringBankHoliday(Day(2009,5,25)))
    assertTrue(Holiday.isSummerBankHoliday(Day(2009,8,31)))
    assertTrue(Holiday.isThanksgiving(Day(2009,11,26)))
  }
}
