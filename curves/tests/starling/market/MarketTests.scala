package starling.market

import starling.utils.StarlingTest
import starling.daterange.{Day, Month}
import starling.daterange.Day._
import org.testng.annotations.Test
import org.mockito.Mockito._
import starling.calendar.{BusinessCalendars, HolidayTablesFactory, HolidayTables}
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.quantity.utils.QuantityTestUtils._
import org.scalatest.testng.TestNGSuite

class MarketTests extends TestMarketTest with TestNGSuite {
  @Test
  def testExpiry {
    val march = Month(2010, 3)
    val delivery = march.thirdWednesday
    val zinc = Market.LME_ZINC
    val expiry = march.firstWednesday
    assertEquals(zinc.optionExpiry(delivery), expiry)
  }

  @Test
  def testFrontMonth {
    val lastTradingDay = Day(2010, 6, 28)
    val hgc = Market.COMEX_HIGH_GRADE_COPPER
    val period = hgc.frontPeriod(lastTradingDay)
    assertEquals(lastTradingDay, hgc.lastTradingDay(period))
  }

  @Test
  def testBadLastTradingDay {
    val lastTradingDay = Day(2010, 5, 28)
    val hgc = Market.COMEX_HIGH_GRADE_COPPER
    assertFalse(hgc.validLastTradingDay(lastTradingDay))
    // should still give a front month
    assertEquals(hgc.frontPeriod(lastTradingDay), Month(2010, 6))
  }

  @Test
  def testLMEOption {
    val mkt = Market.LME_ALUMINIUM
    val jan = Month(2010, 1)
    val observationDays = mkt.observationDays(jan)
    val feb = Month(2010, 2)

    // for the whole observation period of Jan we should see vol sensitivity to first wed of Jan and Feb
    assertEquals(Set(20 Jan 2010, 17 Feb 2010), Set() ++ observationDays.map(mkt.frontOptionPeriod))

    val firstWednesday = jan.firstWednesday
    // after the first wed we should see vol sensitivity to first wed of Feb
    assertEquals(Set(17 Feb 2010), Set() ++ observationDays.filter(_ > firstWednesday).map(mkt.frontOptionPeriod))

  }

  @Test
  def testConversionsOfPowers{
    val twoBBL = Quantity(2.0, BBL)
    val mkt = Market.NYMEX_WTI
    val ratio = mkt.convertUOM(twoBBL, MT) / twoBBL
    val fourBBLSquared = twoBBL * twoBBL
    val squareRatio = mkt.convertUOM(fourBBLSquared, MT * MT) / fourBBLSquared
    assertQtyEquals(squareRatio, ratio * ratio, 1e-6)
  }

  @Test
  def testConversionsOfPowersInverse{
    val twoMT = Quantity(2.0, MT)
    val mkt = Market.NYMEX_WTI
    val ratio = mkt.convertUOM(twoMT, BBL) / twoMT
    val fourMTSquared = twoMT * twoMT
    val squareRatio = mkt.convertUOM(fourMTSquared, BBL * BBL) / fourMTSquared
    assertQtyEquals(squareRatio, ratio * ratio, 1e-6)
  }
}
