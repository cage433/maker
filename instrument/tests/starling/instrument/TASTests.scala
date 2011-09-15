package starling.instrument

import starling.curves._
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}
import starling.market._
import starling.daterange.{DayAndTime, Day, Month}

class TASTests extends TestMarketTest {

  import org.testng.annotations._
  import org.testng.Assert._

  lazy val market = Market.NYMEX_WTI
  val period = Month(2009, 1)
  val forward = Quantity(110, USD / BBL)
  val fixed = Quantity(100, USD / BBL)
  val v = Quantity(1000, BBL)

  def environment(day: DayAndTime) = {
    Environment(UnitTestingAtomicEnvironment(day, {
      case ForwardPriceKey(market, period, _) => forward
      case MarketFixingKey(market, ltd, period) => fixed
    }))
  }

  @Test
  def testNoValueBeforeMaturityAndLikeFutureAfterMaturity() {

    val ltd = market.lastTradingDay(period)

    val maturityDay = ltd - 1
    val strike = environment(maturityDay.endOfDay).priceOnDay(market, period, maturityDay)
    assertQtyEquals(strike, fixed)
    val future = Future(market, period, fixed, v)
    val tas = TAS(market, period, maturityDay, v)

    assertQtyEquals(0.0(USD), tas.mtm(environment(maturityDay.startOfDay)))
    assertQtyEquals(future.mtm(environment(ltd.startOfDay)), tas.mtm(environment(ltd.startOfDay)))
    assertQtyEquals(future.mtm(environment(ltd.endOfDay)), tas.mtm(environment(ltd.endOfDay)))
    assertQtyEquals(future.mtm(environment((ltd + 20).endOfDay)), tas.mtm(environment((ltd + 20).endOfDay)))
  }

  @Test
  def testKeys() {
    val maturityDay = Day(2008, 1, 1)
    val tas = TAS(market, period, maturityDay, v)

    val beforeKeys = tas.atomicMarketDataKeys(maturityDay.startOfDay, USD)
    val afterKeys = tas.atomicMarketDataKeys(maturityDay.endOfDay, USD)

    // we're not actually sensitive to the forward price before the maturity day but it makes the report look a lot better. see comment in TAS
    assertEquals(beforeKeys, Set(ForwardPriceKey(market, period)))
    assertEquals(afterKeys, Set(ForwardPriceKey(market, period), MarketFixingKey(market, maturityDay, period)))
  }

  @Test
  def testExplanation() {
    val maturityDay = Day(2008, 1, 1)
    val tas = TAS(market, period, maturityDay, v)

    val explainBefore = tas.explanation(environment(maturityDay.startOfDay))
    assertEquals(explainBefore.name, "((F - Floating(Fixes.01Jan2008)) * Volume)")
    assertEquals(explainBefore.format(1), "((NYMEX WTI.JAN 2009 - NYMEX WTI.JAN 2009) * 1,000.00 bbl)")
    assertEquals(explainBefore.format(2), "((110.00 USD/bbl - 110.00 USD/bbl) * 1,000.00 bbl)")
    assertEquals(explainBefore.format(2), explainBefore.format(3))

    val explainAfter = tas.explanation(environment(maturityDay.endOfDay))
    assertEquals(explainAfter.name, "((F - K) * Volume)")
    assertEquals(explainAfter.format(1), "((NYMEX WTI.JAN 2009 - NYMEX WTI.JAN 2009(Fixed.01Jan2008)) * 1,000.00 bbl)")
    assertEquals(explainAfter.format(2), "((110.00 USD/bbl - 100.00 USD/bbl) * 1,000.00 bbl)")
    assertEquals(explainAfter.format(2), explainAfter.format(3))
  }
}
