package starling.market.rules

import starling.market.TestMarketTest
import starling.market.Market._
import starling.daterange.Month
import org.testng.Assert._
import org.testng.annotations.Test
import starling.calendar.BusinessCalendarSet
import org.scalatest.testng.TestNGSuite

class SwapPricingRuleTests extends TestMarketTest with TestNGSuite {

  @Test
  def testCommon {
    val period = Month(2011, 1)
    val markets = List(NYMEX_WTI, ICE_BRENT)

    // any days they observe in common are pricing days.
    val allHolidays = NYMEX_WTI.businessCalendar.asInstanceOf[BusinessCalendarSet].holidays.filter(_.month == 1).union(ICE_BRENT.businessCalendar.asInstanceOf[BusinessCalendarSet].holidays.filter(_.month == 1))

    // pricing days is is a weekday and any day that is a holiday in *either* market
    val commonDays = period.days.filter(d => d.isWeekday && !allHolidays.contains(d))
    
    assertEquals(CommonPricingRule.observationDays(markets.map(_.businessCalendar), period), commonDays)
  }
  
  @Test
  def testNonCommon {
    val period = Month(2011, 1)
    val markets = List(NYMEX_WTI, ICE_BRENT)

    // any day that is a pricing day in either market is a pricing day for non-common
    val intersectionHols = NYMEX_WTI.businessCalendar.asInstanceOf[BusinessCalendarSet].holidays.filter(_.month == 1).intersect(ICE_BRENT.businessCalendar.asInstanceOf[BusinessCalendarSet].holidays.filter(_.month == 1))

    // pricing days is is a weekday and any day that is a holiday in *both* markets
    val pricingDays = period.days.filter(d => d.isWeekday && !intersectionHols.contains(d))
    assertEquals(NonCommonPricingRule.observationDays(markets.map(_.businessCalendar), period), pricingDays)
  }
}
