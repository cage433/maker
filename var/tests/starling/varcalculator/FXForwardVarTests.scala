package starling.varcalculator

import starling.utils.StarlingTest
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import starling.instrument.{FXForward, CashInstrument}
import org.testng.annotations.Test
import starling.curves.TestEnvironmentBuilder
import starling.maths.{MatrixUtils, AcklamInverseNormal}
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.daterange.Day
import org.testng.Assert._
import starling.utils.conversions.RichColtMatrices._

class FXForwardVarTests extends StarlingTest {
  @Test
  def testFXForwardVar{
    val marketDay = Day(2009, 10, 1).endOfDay
    val envBuilder = TestEnvironmentBuilder(marketDay)
    val usdPerGbp = Quantity(0.8, USD / GBP)
    val usdPerEur = Quantity(1.1, USD / EUR)
    val usdPerCad = Quantity(0.95, USD / CAD)

    envBuilder.setUSDPerCCYSpotRate(GBP, usdPerGbp)
    envBuilder.setUSDPerCCYSpotRate(EUR, usdPerEur)
    envBuilder.setUSDPerCCYSpotRate(CAD, usdPerCad)
    val usdZeroRate = 0.0
    val gbpZeroRate = 0.0
    val eurZeroRate = 0.0
    envBuilder.setZeroRate(USD, usdZeroRate)  // Note that this rate is irrelevant to this instruments value
    envBuilder.setZeroRate(GBP, gbpZeroRate)
    envBuilder.setZeroRate(CAD, gbpZeroRate)
    envBuilder.setZeroRate(EUR, eurZeroRate)
    val env = envBuilder.build

    val prices = new DVector(Array(usdPerCad.value))
    val vols = new DVector(Array(0.15))
    val rhoMatrix = MatrixUtils.rhoMatrixWithConstantOffDiagonal(1, 0.0)
    val riskFactors = List[VaRRiskFactor](SpotFXRiskFactor(CAD))
    val dT: Double = VarConstants.dT
    val scenGen = new RiskFactorScenarioGenerator(env, riskFactors, prices, vols, rhoMatrix, 12345)

    val strike = Quantity(10.2, USD / CAD)
    val volume = Quantity(3227776, CAD)
    val maturityDate = Day(2010, 10, 1)
    val inst = new FXForward(strike, volume, maturityDate).asUtpPortfolio
    val cadCash = new CashInstrument(volume, maturityDate)
    val usdCash = new CashInstrument(-volume * strike, maturityDate)
    val instruments = List(usdCash, cadCash, inst)
    val varResults = VarInstrumentResults.build(scenGen, instruments, 2000)


    val inverseCDF_95 = AcklamInverseNormal.invoke(0.05)
    val vol = vols(0)
    val price = prices(0)
    import math._
	  var expectedVar95 = volume * usdPerCad * (exp(inverseCDF_95 * vol * sqrt(dT) - 0.5 * vol * vol * dT) - 1.0)
    assertEquals(varResults.VaR(inst, 0.95), expectedVar95.value, expectedVar95.value.abs * 0.04)
    assertEquals(varResults.VaR(inst, 0.95), varResults.VaR(cadCash, 0.95), expectedVar95.value.abs * 0.01)
  }

}
