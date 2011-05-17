package starling.varcalculator

import starling.utils.StarlingTest
import starling.quantity.Quantity._
import starling.quantity.UOM._
import org.testng.annotations.{Test, DataProvider}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import collection.SortedSet
import starling.instrument._
import org.testng.Assert._
import Math._
import starling.curves.interestrate.{DayCountActual365}
import starling.maths.{AcklamInverseNormal, RandomVariables, MatrixUtils}
import starling.calendar.NilCalendar
import starling.db.MetalsPriceTable
import starling.market.{ForwardMarket, FuturesMarket, Market}
import starling.curves.{Environment, InverseConstantInterpolation, TestEnvironmentBuilder}
import starling.quantity.{UOM, Percentage, Quantity}
import starling.daterange.{DayAndTime, Month, Day}


/**
 * Converts any instrument into an equivalent one that is valued in dollars
 */
case class DollarValuedInstrument(inst : Instrument) extends Instrument{
  
  def pivotUTPType = throw new Exception("Unimplemented")

  def valuationCCY = USD

  def assets(env: Environment) = Assets(
    inst.assets(env).assets.map(asset=>
      asset.copyMtm(asset.mtm * env.spotFXRate(UOM.USD, asset.mtm.uom))
    ))
}

class ParametricVarTests extends StarlingTest {

  val marketDay = Day(2009, 11, 2)
  val envBldr = TestEnvironmentBuilder(marketDay.endOfDay)
  val market1 = FuturesMarket.testMarket("Fred", USD, MT)
  val forwardMarket1 = ForwardMarket.testMarket("Fred", USD, MT)
  val market2 = FuturesMarket.testMarket("Ginger", USD, MT)
  envBldr.setConstantPrice(market1, 50.0)
  envBldr.setConstantPrice(market2, 85.0)
  envBldr.setUSDPerCCYSpotRate(EUR, Quantity(1.3, USD / EUR))
  envBldr.setZeroRate(EUR, 0.05)
  envBldr.setZeroRate(USD, 0.05)
  val env = envBldr.build
  val nScenarios = 1000

  /*
    These tests are a little flaky. There is no reason to expect monte carlo VaR results to match in the case of either
      a) non-linearity - this is true for all interest rate sensitivities
      b) cross terms - fx forwards are particularly sensitive to this
   */
  @DataProvider(name = "compareVarsProvider")
  def compareVarsProvider : Array[Array[List[Instrument]]]= {
    Array(
     Array(List[Instrument](new CashInstrument(100 (USD), Day(2010, 11, 30)))),
      Array(List[Instrument](Future(market1, Day(2010, 5, 11), Quantity(77, USD / MT), Quantity(1.0, MT)))),
      Array(List(
        Future(market1, Day(2010, 5, 1), Quantity(100, USD / MT), Quantity(1.0, MT)),
        Future(market2, Day(2010, 5, 1), Quantity(100, USD / MT), Quantity(1.0, MT))
        )),
      Array(
        List[Instrument](FXForward(Quantity(1.2, EUR / USD), Quantity(100, USD), Day(2011, 1, 1)).asUtpPortfolio)
      ),
      Array(
        List[Instrument](CommodityForward(forwardMarket1, Day(2010, 5, 31), Quantity(50, USD/MT), Quantity(555, MT)))
      ),
      Array(
        List[Instrument](
          CommodityForward(forwardMarket1, Day(2010, 5, 5), Quantity(20, USD/MT), Quantity(555, MT)),
          FXForward(Quantity(1.2, EUR / USD), Quantity(100, USD), Day(2010, 1, 1)).asUtpPortfolio,
          Future(market1, Day(2010, 5, 1), Quantity(100, USD / MT), Quantity(1.0, MT)),
          Future(market2, Day(2010, 5, 1), Quantity(100, USD / MT), Quantity(1.0, MT))
        )
      )
    )
  }
  @Test(dataProvider = "compareVarsProvider")
  def compareVars(trades : List[Instrument]){
    val riskFactors = RiskFactorUtilsForTrades.varRiskFactorsForTrades(trades, env, USD).toList
    var (prices, vols, rhoMatrix) = MatrixUtils.randomPriceStatistics(riskFactors.size, 12345)
    prices = new DVector(riskFactors.map(_.representativePriceForVaR(env).value).toArray)
    val scenGen = RiskFactorScenarioGenerator.buildRandomScenarioGenerator(
      marketDay,
      riskFactors,
      prices,
      vols,
      rhoMatrix,
      12345
    )
    val varResults = VarInstrumentResults.build(scenGen, trades.toList, nScenarios)
    val mcVar95 = varResults.VaR(0.95).value
    val mcStdErr = varResults.standardError(0.95).value

    val composite = CompositeInstrument(trades.map(DollarValuedInstrument(_)))
    val deltas = new DVector(riskFactors.map{ rf => composite.riskFactorDerivative(env, rf, composite.valuationCCY).value}.toArray)
    val dT = VarConstants.dT
    val pVar95 = ParametricVar.calculate(prices, deltas, vols, rhoMatrix, dT, 0.95)
    assertEquals(pVar95, mcVar95, 4.0 * mcStdErr)
  }
}
