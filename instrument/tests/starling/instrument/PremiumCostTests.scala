package starling.instrument

import org.testng.annotations.Test
import starling.market.Market
import starling.models.{Call, European}
import starling.curves.{NullAtomicEnvironment, Environment}
import starling.daterange.{DayAndTime, Day, Month}
import starling.utils.StarlingTest
import starling.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}

class PremiumCostTests extends StarlingTest{

  @Test
  def testTradeWithPremiumEqualToValueIsworthZero{
    val mkt = Market.NYMEX_WTI
    val mth = Month(2011, 10)
    val marketDay: DayAndTime = Day(2010, 1, 1).endOfDay
    val env = Environment(new NullAtomicEnvironment(marketDay))
    val atmPrice = env.forwardPrice(mkt, mth)
    for (volume <- List(Quantity(-100, mkt.uom), Quantity(987, mkt.uom))){
      val option = FuturesOption(mkt, mth.firstDay - 5, mth, atmPrice, volume, Call, European)
      val mtm = option.mtm(env)
      val premiumPrice = mtm / volume
      val premium = PremiumCosts(marketDay.day, "fred", volume, premiumPrice)

      assertQtyEquals(mtm + premium.mtm(env, UOM.USD), Quantity(0, UOM.USD), 1e-6)
    }
  }
}
