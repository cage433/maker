package starling.instrument

import starling.utils.StarlingTest
import starling.curves._
import starling.daterange.{Day, Month}
import starling.utils.ScalaTestUtils._
import starling.quantity.UOM._
import starling.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}
import starling.market._
import starling.varcalculator.ForwardPriceRiskFactor

class FuturesTests extends TestMarketSpec {
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
      USDFXRateKey(EUR) -> fxRate,
      FixingKey(Index.WTI10, Market.NYMEX_WTI.lastTradingDay(Month(2009, 1))) -> fixing
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

  //dont @Test  Futures have no mtm when they expire at the moment 
  def testExpiredFuture() {
    val v = Quantity(1000, UOM.BBL)
    val strike = Quantity(20, UOM.USD/UOM.BBL)
    val future = Future(Market.NYMEX_WTI, Month(2009, 1), strike, v)
    val forwardEnv = env.forwardState((marketDay + 100).endOfDay)
    val mtm = future.mtm(forwardEnv)
    assertQtyEquals(mtm, (fixing - strike) * v, 1e-6)

  }

  @Test
  def testParallelShiftDelta {
    val parallelDelta = inst.riskFactorTypePosition(env, ForwardPriceRiskFactorType(mkt), inst.valuationCCY)
    assertEquals(parallelDelta.uom, MT)
    val fp = mkt.frontPeriod(marketDay).asInstanceOf[Day]
    val delta = inst.riskFactorPosition(env, ForwardPriceRiskFactor(mkt, d - fp, d - fp), inst.valuationCCY)
    assertEquals(parallelDelta, delta)
  }

	@Test
	def testFuturesMtmUnderPerturbedEnvironment{
		val dP = Quantity(1.5, mkt.priceUOM)
		val shifts : Map[AtomicDatumKey, Quantity] = Map.empty + (ForwardPriceKey(mkt, d) -> dP)
		val shiftedEnv = Environment(AtomicEnvironmentWithPerturbationMap(atomicEnv, shifts))
		val delta = ( inst.mtm(shiftedEnv) - inst.mtm(env) ) / dP
		assertQtyEquals(delta, volume, 1e-6)
	}
}
