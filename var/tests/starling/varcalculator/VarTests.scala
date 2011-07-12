package starling.varcalculator

import org.scalatest.testng.TestNGSuite
import starling.utils.StarlingTest
import starling.maths.{AcklamInverseNormal, RandomCorrelationMatrix}
import org.testng.annotations._
import org.testng.Assert._
import starling.utils.ScalaTestUtils._
import starling.quantity.UOM._
import starling.maths.MatrixUtils._
import starling.utils.conversions.RichColtMatrices._
import java.lang.Math._
import starling.utils.QuantityTestUtils._
import starling.curves._
import starling.instrument.{FuturesOption, Future, Instrument}
import cern.colt.matrix.DoubleFactory2D
import starling.quantity.{Percentage, Quantity}
import starling.models.{European, Call}
import starling.market.{ForwardPriceRiskFactorType, TestMarketSpec, FuturesMarket, Market}
import starling.daterange.{Month, TestHolidays, Day}

class VarTests extends TestMarketSpec {
  val market1 = Market.testMarketWithInterpolation("MKT 1", USD, MT)
  val market2 = Market.testMarketWithInterpolation("MKT 2", USD, MT)
  val marketDay = Day(2009, 1, 1)
  val seed = 1234

  @Test
  /** Tests that var for a futures contract lying between two others has the expected VAR
   */
  def testSingleInstrumentVarMidPrompt{
    val promptDays = List(Day(2009, 10, 1), Day(2009, 11, 1))
    val prices = List(50.0, 100.0)
    val vol = 0.5
    val vols = List(vol, vol)
	  val dT = VarConstants.dT
	  val rho = 0.9999999
	  val rhoMatrix = rhoMatrixWithConstantOffDiagonal(2, rho)

    val curve = ForwardCurve.create(
      market1, marketDay.endOfDay, promptDays.zip(prices).toMap
      )

    val env = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 1, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(_, day, _) => curve.price(day)
          case _ : DiscountRateKey => 1.0
        }
      }
    )
	  val deliveryDay = Day(2009, 10, 15)
	  val future = Future(market1, deliveryDay, Quantity(0.0, USD / MT), Quantity(1.0, MT))
    val riskFactors = promptDays.map(d => ForwardPriceRiskFactor(market1, d - marketDay, d - marketDay))
    val scenGen = RiskFactorScenarioGenerator(env, riskFactors, prices, vols, rhoMatrix, seed)

    val nScenarios = 1000
    val varResults = VarUtpResults.build(scenGen, List[Instrument](future), nScenarios)
    val observedVar95 = varResults.VaR(0.95)
    val stdErr = varResults.standardError.value
    val price = env.forwardPrice(market1, deliveryDay).value
    val inverseCDF_95 = AcklamInverseNormal.invoke(0.05)
	  val expectedVar95 = Quantity(price * (exp(inverseCDF_95 * vol * sqrt(dT) - 0.5 * vol * vol * dT) - 1.0), USD)
	  assertQtyEquals(observedVar95, expectedVar95, 3.0 * stdErr)
  }

  def volOfAverage(vol1 : Double, vol2 : Double, a : Double, b : Double, rho : Double) : Double = {
	    math.sqrt(a * a * vol1 * vol1 + b * b * vol2 * vol2 + 2.0 * a * b * rho * vol1 * vol2)
  }

  def weightedVol(days : List[Day], vols : List[Double], rho : Double, day : Day) : Double = {
  		var dayVols = days.zip(vols)
  		val indexFirstDayAfter = dayVols.indexWhere(_._1 >= day)
  		if (indexFirstDayAfter == 0)
  			dayVols = dayVols.take(2)
  		else
  			dayVols = dayVols.drop(indexFirstDayAfter - 1).take(2)
  		assert(dayVols.size == 2, "Invalid size")
  		(dayVols : @unchecked) match {
  		  case List((day1, vol1), (day2, vol2)) => {
  		    val tMid = day.endOfDay.timeSince(day1.endOfDay)
  		    val tWidth = day2.endOfDay.timeSince(day1.endOfDay)
  		    val a = 1.0 - tMid / tWidth
  		    val b = tMid / tWidth
  		    volOfAverage(vol1, vol2, a, b, rho)
  		  }
      }
  }

  @Test
  /** Tests that var for a futures contract lying between two others has the expected VAR
   * 	when the surrounding price points are less than perfectly correlated.
   */
  def testSingleInstrumentVarMidPromptUncorrelated{
    val promptDays = List(Day(2009, 10, 1), Day(2009, 11, 1))
    val price: Double = 100.0
    val prices = List(price, price)
    val vols = List(0.5, 0.3)
	  val dT = VarConstants.dT
	  val rho = 0.5
	  val rhoMatrix = rhoMatrixWithConstantOffDiagonal(2, rho)

    val env = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 1, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => price
          case _ : DiscountRateKey => 1.0
        }
      }
    )
    val riskFactors = promptDays.map(d => ForwardPriceRiskFactor(market1, d - marketDay, d - marketDay))
    val scenGen = RiskFactorScenarioGenerator(env, riskFactors, prices, vols, rhoMatrix, seed)

    val deliveryDay = Day(2009, 10, 5)
    val future = Future(market1, deliveryDay, Quantity(0.0, USD / MT), Quantity(1.0, MT))

    val varResults = VarUtpResults.build(scenGen, List[Instrument](future), 1000)
    val observedVar95 = varResults.VaR(0.95)
	  val stdErr = varResults.standardError.value
    val inverseCDF_95 = AcklamInverseNormal.invoke(0.05)

	  val v = weightedVol(promptDays, vols, rho, deliveryDay)
	  val expectedVar95 = Quantity(price * (exp(inverseCDF_95 * v * sqrt(dT) - 0.5 * v * v * dT) - 1.0), USD)
	  assertQtyEquals(observedVar95, expectedVar95, 3.0 * stdErr)
  }

  @Test
  /** Test that when producing var using two market's curves, the var for a single market's instrument is as expected
   */
  def testOneInstVarWithTwoMarketScenGenerator{
    val promptDays = List(Day(2009, 10, 1), Day(2009, 11, 1))

    val price: Double = 100.0
    val prices = List(price, price)
    val vols = List(0.5, 0.3)
	  val dT = VarConstants.dT
	  val rho = 0.5
	  val rhoMatrix = rhoMatrixWithConstantOffDiagonal(4, rho)

    val env = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 1, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => price
          case _ : DiscountRateKey => 1.0
        }
      }
    )

	  val deliveryDay = Day(2009, 11, 1)
	  val future1 = Future(market1, deliveryDay, Quantity(0.0, USD / MT), Quantity(1.0, MT))
	  val future2 = Future(market2, deliveryDay, Quantity(0.0, USD / MT), Quantity(1.0, MT))

    val riskFactors = for (
        mkt <- List(market1, market2);
        d <- promptDays
    ) yield ForwardPriceRiskFactor(mkt, d - marketDay, d - marketDay)

    val scenGen = RiskFactorScenarioGenerator(env, riskFactors, prices ++ prices.map(_ * 0.5), vols ++ vols.map(_ * 0.5), rhoMatrix, seed)
    val varResults = VarUtpResults.build(scenGen, List[Instrument](future1), 1000)
	  val observedVar95 = varResults.VaR(0.95)
	  val stdErr = varResults.standardError
    val inverseCDF_95 = AcklamInverseNormal.invoke(0.05)
	  val v = weightedVol(promptDays, vols, rho, deliveryDay)

	  val expectedVar95 = Quantity(price * (exp(inverseCDF_95 * v * sqrt(dT) - 0.5 * v * v * dT) - 1.0), USD)
	  assertQtyEquals(observedVar95, expectedVar95, 3.0 * stdErr.value)
  }

  @Test
  /** Test that when producing var using two market's curves, the var for a single market's instrument is as expected
   */
  def testTwoInstVarWithTwoMarketScenGenerator{
    val promptDays = List(Day(2009, 10, 1), Day(2009, 11, 1))
    val price = 100.0
    val prices = List(price, price)
    val vols = List(0.5, 0.3)
	  val dT = VarConstants.dT
	  val rho = 0.5
	  val rhoMatrix = rhoMatrixWithConstantOffDiagonal(4, rho)
    val env = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 1, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => price
          case _ : DiscountRateKey => 1.0
        }
      }
    )

    val riskFactors = for (
        mkt <- List(market1, market2);
        d <- promptDays
    ) yield ForwardPriceRiskFactor(mkt, d - marketDay, d - marketDay)
    val scenGen = RiskFactorScenarioGenerator(env, riskFactors, prices ++ prices, vols ++ vols, rhoMatrix, seed)

    val deliveryDay = Day(2009, 11, 1)
    val future1 = Future(market1, deliveryDay, Quantity(0.0, USD / MT), Quantity(1.0, MT))
	  val future2 = Future(market2, deliveryDay, Quantity(0.0, USD / MT), Quantity(-1.0, MT))

	  def compareExpectedAndActualVar(trades : List[Instrument], weightedVolatility : Double, volume : Double){
      val nScenarios = 50000
      val varResults = VarInstrumentResults.build(scenGen, trades, nScenarios)
      val observedVar95 = varResults.VaR(0.95)
      val stdErr = varResults.standardError.value
    	val v = weightedVolatility
      val inverseCDF_95 = AcklamInverseNormal.invoke(0.05)
    	val expectedVar95 = Quantity(price * (exp(inverseCDF_95 * v * sqrt(dT) - 0.5 * v * v * dT) - 1.0) * volume, USD)
    	assertQtyEquals(observedVar95, expectedVar95, 3.0 * stdErr)

	  }
	  val vSameSignTrades = weightedVol(promptDays, vols, rho, deliveryDay)
    compareExpectedAndActualVar(List[Instrument](future1, future1), vSameSignTrades, 2.0)
  }


//  @Test
  /**
   * The 'fix' for this caused much wierdness in trinity VaR. Fix has been removed until this is invesitgated.
   */
  def testHedgeOptionPortfolioHasNegativeVar{

    val price = 95.0
    val vol = 0.5
    val env = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 10, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => Quantity(price, USD/MT)
          case _ : BradyMetalVolAtomicDatumKey => new Percentage(vol)
          case _ : OilAtmVolAtomicDatumKey => Percentage(vol)
          case _ : OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
          case _ : DiscountRateKey => 1.0
        }
      }
    )

    val option = FuturesOption(market1, Day(2010, 1, 1), Day(2010, 1, 2), Quantity(100, USD/MT), Quantity(10, MT), Call, European)
    val position = option.riskFactorTypePosition(env, ForwardPriceRiskFactorType(market1), USD)
    val hedge = Future(market1, Day(2010, 1, 2), Quantity(10, USD/MT), -position)

    val riskFactors = option.varRiskFactors(env, USD)
    assertEquals(riskFactors.size, 1)

    val rhoMatrix = DoubleFactory2D.dense.identity(1)
    val scenGen = RiskFactorScenarioGenerator(env, riskFactors.toList, List(price), List(vol), rhoMatrix, seed)
    val trades = List(option, hedge)
    val varResults = VarInstrumentResults.build(scenGen, trades, 10000)
    val observedVar95 = varResults.VaR(0.95).value

    assertTrue(observedVar95 < 0, "Var is positive: " + observedVar95)
  }

  @Test
  def testOptionWithMissingMarketDataFallsBackToPreviousDay {

    val price = 95.0
    val vol = 0.5
    val env1 = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 10, 2).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => Quantity(price, USD/BBL)
          case _ : OilAtmVolAtomicDatumKey => Percentage(vol)
          case _ : OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
          case _ : DiscountRateKey => 1.0
        }
      }
    )
    val env2 = Environment(
      new TestingAtomicEnvironment(){
        def marketDay =  Day(2009, 10, 5).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => Quantity(price, USD/BBL)
          case _ : DiscountRateKey => 1.0
        }
      }
    )

    val market = Market.NYMEX_WTI
    val option = FuturesOption(market, Day(2010, 1, 2), Month(2010, 2), Quantity(100, USD/BBL), Quantity(10, BBL), Call, European)
    val riskFactors = option.varRiskFactors(env2, USD)

    val rhoMatrix = DoubleFactory2D.dense.identity(1)
    val scenGen = RiskFactorScenarioGenerator(env2, riskFactors.toList, List(price), List(vol), rhoMatrix, seed, Some(env1))

    val trades = List(option)
    val varResults = VarInstrumentResults.build(scenGen, trades, 10000)
    val observedVar95 = varResults.VaR(0.95).value

    assertEquals(varResults.instErrors, List())
    assertTrue(observedVar95 < 0, "Var is positive: " + observedVar95)
  }
}
