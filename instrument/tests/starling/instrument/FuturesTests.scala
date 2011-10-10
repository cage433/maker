package starling.instrument

import starling.utils.StarlingTest
import starling.curves._
import starling.utils.ScalaTestUtils._
import starling.quantity.UOM._
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}
import starling.market._
import starling.daterange.{DayAndTime, DayAndNoTime, Day, Month}

class FuturesTests extends TestMarketTest {
	import org.testng.annotations._
	import org.testng.Assert._

	val lotSize = 100;
	val mkt = Market.testMarket("Test", EUR, MT)
	val d = Day(2009, 12, 10)
	val price = Quantity(99.0, mkt.priceUOM)
  val fxRate = Quantity(.5, USD/EUR)
  val fixing = Quantity(30.0, Market.NYMEX_WTI.priceUOM)
  val marketDay: Day = Day(2009, 9, 9)
  val atomicEnv = MappingAtomicEnvironment(
	  Map(
      ForwardPriceKey(mkt, d) -> price,
      USDFXRateKey(EUR) -> fxRate
    ),
   marketDay.endOfDay
	)
	val env = Environment (atomicEnv)
	val K = Quantity(105.0, mkt.priceUOM)
	val numLots = 10;
  val volume = new Quantity(numLots * lotSize, MT)
  val inst = Future(mkt, d, K, volume)

	@Test
	/**
	* Trivial test that futures valuation is as expected. 
	*/
	def testFuturesMtm{
		val mtm = inst.mtm(env)
		assertQtyEquals(mtm, (price - K) * volume, 1e-6)
	}

  @Test
  def testExpiredFuture() {
    val market = Market.NYMEX_WTI
    val period = Month(2009, 1)
    val ltd = market.lastTradingDay(period)

    val forward = Quantity(110, USD / BBL)
    val fixed = Quantity(100, USD / BBL)

    def environment(day:DayAndTime) = {
      Environment(UnitTestingAtomicEnvironment(day, {
        case ForwardPriceKey(market, period, _) => forward
        case MarketFixingKey(market, ltd, period) => fixed
      }))
    }
    val v = Quantity(1000, UOM.BBL)
    val strike = Quantity(20, UOM.USD/UOM.BBL)
    val future = Future(market, period, strike, v)

    val mtmLive = future.mtm(environment(ltd.startOfDay))
    val mtmLtd = future.mtm(environment(ltd.endOfDay))
    val mtmLater = future.mtm(environment((ltd + 20).endOfDay))

    assertQtyEquals(mtmLive, (forward - strike) * v)
    assertQtyEquals(mtmLtd, (fixed - strike) * v)
    assertQtyEquals(mtmLtd, mtmLater)
    assertNotSame(mtmLive, mtmLtd)
  }

	@Test
	def testFuturesMtmUnderPerturbedEnvironment{
		val dP = Quantity(1.5, mkt.priceUOM)
		val shifts : Map[AtomicDatumKey, Quantity] = Map.empty + (ForwardPriceKey(mkt, d) -> dP)
		val shiftedEnv = Environment(AtomicEnvironmentWithPerturbationMap(atomicEnv, shifts))
		val delta = ( inst.mtm(shiftedEnv) - inst.mtm(env) ) / dP
		assertQtyEquals(delta, volume, 1e-6)
	}

  @Test
  def testExplanation() {
    val explain = inst.explanation(env)
    assertEquals(explain.name, "((F - K) * Volume)")
    assertEquals(explain.format(1), "((Test.10Dec2009 - 105.00 EUR/MT) * 1,000.00 MT)")
    val ex = "((99.00 EUR/MT - 105.00 EUR/MT) * 1,000.00 MT)"
    assertEquals(explain.format(2), ex)
    assertEquals(explain.format(3), ex)
  }
}
