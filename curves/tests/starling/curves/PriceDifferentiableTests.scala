package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.daterange.{Month, DayAndTime, Day}
import starling.market.Market

class PriceDifferentiableTests extends StarlingTest{
  @Test
  def testQuantityValueForDailyMarket{
    val marketDay: DayAndTime = Day(2010, 1, 1).endOfDay
    val env = Environment(new NullAtomicEnvironment(marketDay))
    val diff = new PriceDifferentiable(Market.PREM_UNL_EURO_BOB_OXY_NWE_BARGES, Month(2010, 12))
    val price = diff.quantityValue(env)
    println("Price = " + price)
  }
}