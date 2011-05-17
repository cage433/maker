package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.market.PublishedIndex
import starling.daterange.{Month, DayAndTime, Day}

class PriceDifferentiableTests extends StarlingTest{
  @Test
  def testQuantityValueForDailyMarket{
    val marketDay: DayAndTime = Day(2010, 1, 1).endOfDay
    val env = Environment(new NullAtomicEnvironment(marketDay))
    val diff = new PriceDifferentiable(PublishedIndex.PREM_UNL_EURO_BOB_OXY_NWE_BARGES.forwardMarket, Month(2010, 12))
    val price = diff.quantityValue(env)
    println("Price = " + price)
  }
}