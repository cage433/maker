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
    val diff = new PriceDifferentiable(Market.ICE_WTI, Month(2010, 12))
    val price = diff.quantityValue(env)
    println("Price = " + price)
  }
}