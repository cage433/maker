package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.utils.QuantityTestUtils._
import starling.market._
import rules.CommonPricingRule
import starling.curves._
import starling.market.formula._
import starling.daterange._
import starling.daterange.Day._

class SwapCalendarSpreadTests extends TestExpiryRules {

  @Test
  def testSameAsTwoSwaps {
    val forwardPrice = Quantity(200, USD / BBL)
    val historicPrice = Quantity(100, USD / BBL)

    def environment(marketDayAndTime: DayAndTime) = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(_, d, _) => {
            forwardPrice
          }
          case _: FixingKey => historicPrice
        }

        def marketDay = marketDayAndTime
      }
    )

    val index = PublishedIndex.DATED_BRENT
    val market = index.market

    val period1 = DateRange(8 Jan 2011, 20 Jan 2011)
    val period2 = Month(2011, 2)
    val volume = Quantity(100, BBL)
    val strike = Quantity(.9, USD / BBL)

    val swapFront = new SingleCommoditySwap(
      index,
      strike,
      volume,
      period1,
      true
    )

    val swapBack = new SingleCommoditySwap(
      index,
      strike.copy(value = 0.0),
      -volume,
      period2,
      true
    )

    val swap = new SwapCalendarSpread(index, strike, volume, SpreadPeriod(period1, period2), cleared = true)

    for (marketDay <- ((30 Dec 2010) until (11 Mar 2011)).filter(_.isWeekday); timeOfDay <- List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay)) {
      val marketDayAndTime = marketDay.atTimeOfDay(timeOfDay)
      val env = environment(marketDayAndTime)
      val mtm = swap.mtm(env)
      val expectedMTM = swapFront.mtm(env, USD) + swapBack.mtm(env, USD)

      assertQtyEquals(mtm, expectedMTM, 1e-6)

      val keys = swap.priceAndVolKeys(marketDayAndTime)
      val expectedKeys = swapFront.priceAndVolKeys(marketDayAndTime)._1 ++ swapBack.priceAndVolKeys(marketDayAndTime)._1
      assertEquals(keys, (expectedKeys, Set()))

      for (k <- keys._1) {
        val position = swap.position(env, k)
        val expectedPosition = swapFront.position(env, k) + swapBack.position(env, k)
        assertQtyEquals(position, expectedPosition, 1e-6)
      }
    }
  }
}
