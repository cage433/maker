package starling.varcalculator

import starling.utils.StarlingTest
import org.testng.annotations._
import starling.daterange.Day
import starling.quantity.UOM._
import starling.maths.MatrixUtils._
import starling.maths.PriceShiftGenerator
import starling.utils.conversions.RichColtMatrices._
import org.testng.Assert._
import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D =>DVector, DenseDoubleMatrix2D => DMatrix}
import starling.market.{TestExpiryRules, FuturesMarket, Market}

class ScenarioGeneratorTests extends TestExpiryRules {
	val marketDay = Day(2009, 1, 1)
	val nPrompts = 5
	val prompts = (0 until nPrompts).toList
	val promptDays = prompts.map{i => (marketDay.containingMonth + i).lastDay} 
  val nMarkets = 3
  val nPricePoints = nMarkets * prompts.size
		
	def makeMarket(n : Int) : FuturesMarket = FuturesMarket.testMarket("Market " + n, USD, MT)
 
  @Test
  /**
   * Create random historic vol/price/rho data - use it to generate scenarios
   * and check the observed vols/correlations are as expected
   */
	def testSampleVolsAndCorrelations{
	  val markets = (0 until nMarkets).toList.map(makeMarket)
	  
	  val dT = VarConstants.dT
	  val seed = 1234
	  val (prices, vols, rhoMatrix) = randomPriceStatistics(markets.size * nPrompts, seed)
	  val priceShifter = PriceShiftGenerator(prices, vols, rhoMatrix, dT, seed * 2)
    val riskFactors = for (mkt <- markets; day <- promptDays) yield {
      val fp = mkt.frontPeriod(marketDay).asInstanceOf[Day]
      ForwardPriceRiskFactor(mkt, day - fp, day - fp)
    }
	  val scenGen = RiskFactorScenarioGenerator.buildRandomScenarioGenerator(
	  		marketDay, riskFactors, prices, vols, rhoMatrix, 12345  
	  )
	  val nScenarios = 6000
	  val nPricePoints = nPrompts * nMarkets
	  val samples = new DMatrix(nScenarios, nPricePoints)
	  val logReturns = new DMatrix(nScenarios, nPricePoints)
                              
	  for (iScen <- 0 until nScenarios){
	    val env = scenGen.next
	    for (
	      (mkt, iMkt) <- markets.zipWithIndex;
	      iPrompt <- 0 until nPrompts
	    ){
	      val iCol = iMkt * nPrompts + iPrompt
	      samples(iScen, iCol) = env.forwardPrice(mkt, promptDays(iPrompt)).value
	    }
	  }
	  for (iCol <- 0 until nPricePoints){
	    logReturns.viewColumn(iCol) := (samples.viewColumn(iCol) / prices(iCol)).log / math.sqrt(dT)
	  }
	  for (i <- 0 until nPricePoints){
	    assertEquals(logReturns.viewColumn(i).stdDev, vols(i), 2e-2)
	    for (j <- i  until nPricePoints){
	      assertEquals(logReturns.viewColumn(i).correlation(logReturns.viewColumn(j)), rhoMatrix(i, j), 0.04)
	    }
     
	  }
	}
}
