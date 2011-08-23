package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.utils.QuantityTestUtils._
import starling.market._
import starling.curves._
import starling.daterange._
import starling.daterange.Day._
import starling.utils.TestTimer

class SwapCalendarSpreadTests extends TestMarketTest {

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

  @Test
  def testExplanation {
    val forwardPrice = Quantity(200, USD / BBL)
    val historicPrice = Quantity(100, USD / BBL)
    val md = Day(2011, 1, 16).endOfDay
    val zeroRate = 0.3

    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(_, d, _) => {
            forwardPrice
          }
          case _: FixingKey => historicPrice
          case DiscountRateKey(_, day, _) => new Quantity(math.exp(-zeroRate * day.endOfDay.timeSince(md)))
        }

        def marketDay = md
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

    val swap = new SwapCalendarSpread(index, strike, volume, SpreadPeriod(period1, period2), cleared = false)

    val explanation = swap.explanation(env)

    assertEquals(explanation.name, "((((Average(Dated Brent.17Jan2011 - 20Jan2011) - Average(Dated Brent.FEBRUARY 2011)) - K) * Volume) * USD.07Mar2011)")
    val ex = "((((Average(17Jan2011, 18Jan2011, 19Jan2011, 20Jan2011) - Average(01Feb2011, 02Feb2011, 03Feb2011, 04Feb2011, 07Feb2011, 08Feb2011, 09Feb2011, 10Feb2011, 11Feb2011, 14Feb2011, 15Feb2011, 16Feb2011, 17Feb2011, 18Feb2011, 21Feb2011, 22Feb2011, 23Feb2011, 24Feb2011, 25Feb2011, 28Feb2011)) - 0.90 USD/bbl) * 100.00 bbl) * 0.96)"
    assertEquals(explanation.format(1), ex)
    val ex1 = "((((Average(Dated Brent.17Jan2011, Dated Brent.18Jan2011, Dated Brent.19Jan2011, Dated Brent.20Jan2011) - Average(Dated Brent.01Feb2011, Dated Brent.02Feb2011, Dated Brent.03Feb2011, Dated Brent.04Feb2011, Dated Brent.07Feb2011, Dated Brent.08Feb2011, Dated Brent.09Feb2011, Dated Brent.10Feb2011, Dated Brent.11Feb2011, Dated Brent.14Feb2011, Dated Brent.15Feb2011, Dated Brent.16Feb2011, Dated Brent.17Feb2011, Dated Brent.18Feb2011, Dated Brent.21Feb2011, Dated Brent.22Feb2011, Dated Brent.23Feb2011, Dated Brent.24Feb2011, Dated Brent.25Feb2011, Dated Brent.28Feb2011)) - 0.90 USD/bbl) * 100.00 bbl) * 0.96)"
    assertEquals(explanation.format(2), ex1)
    val lastExplanation = "((((Average(200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl) - Average(200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl, 200.00 USD/bbl)) - 0.90 USD/bbl) * 100.00 bbl) * 0.96)"
    assertEquals(explanation.format(3), lastExplanation)
    assertEquals(explanation.format(4), lastExplanation)
  }
}
