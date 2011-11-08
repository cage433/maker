package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.daterange.Day
import starling.quantity.UOM._
import starling.quantity.Percentage
import starling.daterange.Month
import starling.quantity.Quantity
import starling.market.{TestMarketTest, Index, FuturesFrontPeriodIndex, Market}
import org.scalatest.testng.TestNGSuite

class MarketDayChangeEnvironmentTests extends TestMarketTest with TestNGSuite {
  @Test
  def testMarketDayChanges{
    val marketDay = Day(2010, 1, 1).endOfDay
    val mkt = Market.NYMEX_WTI
    val env = UnitTestingEnvironment(
      marketDay, 
      {
        case DiscountRateKey(_, d, _) => new Quantity(math.exp(-0.05 * d.daysSinceInYears(marketDay.day)))
        case _ : OilVolSkewAtomicDatumKey => Map(0.5 -> Percentage(0.05), 0.8 -> Percentage(0.08))
        case _ : OilAtmVolAtomicDatumKey => Percentage(0.2)
        case _ : ForwardPriceKey => Quantity(100.0, mkt.priceUOM)
      }
    )
    val newMarketDay = (marketDay.day + 10).endOfDay
    val shiftedEnv = env.shiftMarketDayAtInstrumentLevel(newMarketDay)
    val day = Day(2010, 10, 10)
    assertEquals(newMarketDay, shiftedEnv.marketDay)

    // discounts will change
    assertNotSame(env.discount(USD, day), shiftedEnv.discount(USD, day))

    // vols should not
    val vol = env.impliedVol(mkt, Month(2010, 8), Day(2010, 7, 7), Quantity(50.0, mkt.priceUOM))
    val shiftedVol = shiftedEnv.impliedVol(Market.NYMEX_WTI, Month(2010, 8), Day(2010, 7, 7), Quantity(50.0, mkt.priceUOM))
    assertEquals(vol, shiftedVol)

    val fixing = shiftedEnv.indexFixing(Index.WTI10, newMarketDay.day)
    val forwardEnv = env.forwardState(newMarketDay)
    assertEquals(fixing, forwardEnv.indexFixing(Index.WTI10, newMarketDay.day))
  }
}
