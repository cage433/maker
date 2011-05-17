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
import starling.utils.QuantityTestUtils._

class MarketTests extends TestExpiryRules {
  @Test
  def testTrinityCodes {
    // Trinity codes taken from before refactor. Test is to make sure nothing changed.
    val eaiToCode = Map(546 → "GC", 1037 → "XZS", 511 → "BPM", 1306 → "BSM",
                        512 → "BCM", 973 → "XCS", 974 → "XAS", 683 → "XPD", 684 → "XPT", 545 → "XHG",
                        1148 → "SAU", 2 → "NCL", 1 → "ICO", 14 → "IGO", 15 → "NHO", 1310 → "NFO", 890 → "ICL")
    for ((eai, code) <- eaiToCode) {
      assertEquals(Market.fromTrinityCode(code) match {
              case f: ForwardMarket => f.eaiQuoteID
              case m: FuturesMarket => m.eaiQuoteID
      }, Some(eai))
    }
  }

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
  def testSteelAndIronOreDiffer{
    assertFalse(Market.STEEL_REBAR_SHANGHAI == Market.IRON_ORE)
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

    assertEquals(Month(2010, 4).thirdWednesday, mkt.nthOptionPeriod((1 Jan 2010).endOfDay, 3))
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
}
