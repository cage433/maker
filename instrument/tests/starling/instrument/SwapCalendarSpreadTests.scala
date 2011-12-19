package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.quantity.utils.QuantityTestUtils._
import starling.market._
import starling.curves._
import starling.daterange._
import starling.daterange.Day._
import starling.utils.TestTimer
import org.scalatest.testng.TestNGSuite

class SwapCalendarSpreadTests extends TestMarketTest with TestNGSuite {

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
          case _: IndexFixingKey => historicPrice
        }

        def marketDay = marketDayAndTime
      }
    )

    val index = Index.DATED_BRENT
    val market = index.market

    val period1 = DateRange(8 Jan 2011, 20 Jan 2011)
    val period2 = Month(2011, 2)
    val volume = Quantity(100, BBL)
    val strike = Quantity(.9, USD / BBL)

    val swapFront = new SinglePeriodSwap(
      index,
      strike,
      volume,
      period1,
      true
    )

    val swapBack = new SinglePeriodSwap(
      index,
      strike.copy(value = 0.0),
      -volume,
      period2,
      true
    )

    val swap = new SwapCalendarSpread(index, strike, volume, SpreadPeriod(period1, period2), cleared = true)

    val edgeDays = (7 Jan 2011) :: (10 Jan 2011) :: (19 Jan 2011) :: (20 Jan 2011) :: (21 Jan 2011) :: //
      (31 Jan 2011) :: (1 Feb 2011) :: (2 Feb 2011) :: (25 Feb 2011) :: (28 Feb 2011) :: (1 Mar 2011) :: Nil
    // ensure each edge day is a market day
    assert(edgeDays.forall(_.isWeekday));

    for (marketDay <- edgeDays; timeOfDay <- List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay)) {
      val marketDayAndTime = marketDay.atTimeOfDay(timeOfDay)
      val env = environment(marketDayAndTime)
      val mtm = swap.mtm(env)
      val expectedMTM = swapFront.mtm(env, USD) + swapBack.mtm(env, USD)

      assertQtyEquals(mtm, expectedMTM, 1e-6)

      val keys = swap.priceAndVolKeys(env)
      val expectedKeys = swapFront.priceAndVolKeys(env)._1 ++ swapBack.priceAndVolKeys(env)._1
      assertEquals(keys, (expectedKeys, Set()))
      for (k <- keys._1) {
        val position = swap.position(env, k)
        val expectedPosition = swapFront.position(env, k) + swapBack.position(env, k)
        assertQtyEquals(position, expectedPosition, 1e-6)
      }
    }
  }

  @Test
  def testPremUnlInBBL {
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market
    val period = Month(2009, 9) / Month(2009, 10)
    val volume = Quantity(1000, BBL)
    val strike = Quantity(200, USD / MT)

    val md = Day(2009, 1, 1)
    val swap = new SwapCalendarSpread(index, strike, volume, period, false)
    val environment = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _: ForwardPriceKey => Quantity(100, USD / MT)
        }

        def marketDay = md.endOfDay
      }
    ).undiscounted

    val keys = swap.asUtpPortfolio(md).portfolio.keys.map {
      utp => utp.priceAndVolKeys(environment)
    }

    assertFalse(keys.isEmpty)
    val mtm = swap.mtm(environment)
    val exp = swap.explanation(environment)

    assertQtyEquals(mtm, exp)
  }
}
