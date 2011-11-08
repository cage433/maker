package starling.instrument

import starling.utils.StarlingTest
import starling.curves._
import starling.quantity.Quantity._
import org.testng.annotations.{DataProvider, Test}
import math._
import starling.models.{Call, Put}
import org.testng.Assert._
import starling.quantity.{Quantity, Percentage}
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.daterange._
import starling.quantity.utils.QuantityTestUtils._
import org.scalatest.testng.TestNGSuite
import starling.maths.StandardNormal
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import cern.colt.matrix.impl.DenseDoubleMatrix1D
import starling.market.{FuturesSpreadMarket, JonTestEnv, Market}

class CommoditySpreadOptionTests extends JonTestEnv with TestNGSuite{

  @Test
  def testDeltaAndGamma{
    val md = Day(2010, 1, 14).endOfDay
    val june = Month(2010, 6)
    val mkt = FuturesSpreadMarket.ICE_WTI_BRENT
    val mkt1 = mkt.market1
    val mkt2 = mkt.market2
    val stdDev = 0.5
    val FWTI = 101.0
    val FBrent = 111.0
    val K = 0.2333
    val env = UnitTestingEnvironment(
      md,
      {key =>
        key match {
          case ForwardPriceKey(`mkt1`, `june`, _) => FWTI(mkt.priceUOM)
          case ForwardPriceKey(`mkt2`, `june`, _) => FBrent(mkt.priceUOM)
          case _ : DiscountRateKey => new Quantity(1.0)
          case _ : SpreadAtmStdDevAtomicDatumKey => stdDev(mkt.priceUOM)
          case _ : SpreadSkewStdDevAtomicDatumKey => new DenseDoubleMatrix1D(Array(0.0, 0.0)).asRowMatrix
        }
      }
    )
    val exerciseDay = mkt.spreadOptionExpiry(june)
    val T = exerciseDay.endOfDay.timeSince(md)
    val volume = 10.0(mkt.uom)
    val cso = new CommoditySpreadOption(mkt, june, K(mkt.priceUOM), volume, Put)
    val gamma = cso.gamma(env, FuturesSpreadPrice(mkt, june), USD, List(FuturesSpreadPrice(mkt, june)), multiple = 1e-3).value
    val delta = cso.firstOrderDerivative(env, FuturesSpreadPrice(mkt, june), USD, multiple = 1e-3).value
    val scaledVol = stdDev * sqrt(T)
    val spread = FWTI - FBrent
    val d1 = (spread - K) / scaledVol
    val n1 = StandardNormal.pdf(d1)
    val N1 = StandardNormal.cdf(d1)
    val expectedDelta = volume.value * (N1 - 1)
    assertEquals(delta, expectedDelta, 1e-4)
    val expectedGamma = volume.value * n1 / scaledVol
    assertEquals(gamma, expectedGamma, 1e-4)
  }

  @Test
  def testExplanation{
    val md = Day(2010, 1, 14).endOfDay
    val june = Month(2010, 6)
    val mkt = FuturesSpreadMarket.ICE_WTI_BRENT
    val mkt1 = mkt.market1
    val mkt2 = mkt.market2
    val stdDev = 0.5
    val FWTI = 101.0
    val FBrent = 111.0
    val K = 0.2333
    val zeroRate = 0.1
    val env = UnitTestingEnvironment(
      md,
      {key =>
        key match {
          case ForwardPriceKey(`mkt1`, `june`, _) => FWTI(mkt.priceUOM)
          case ForwardPriceKey(`mkt2`, `june`, _) => FBrent(mkt.priceUOM)
          case DiscountRateKey(_, day, _) => new Quantity(math.exp(- zeroRate * day.endOfDay.timeSince(md)))
          case _ : SpreadAtmStdDevAtomicDatumKey => stdDev(mkt.priceUOM)
          case _ : SpreadSkewStdDevAtomicDatumKey => new DenseDoubleMatrix1D(Array(0.0, 0.0)).asRowMatrix
        }
      }
    )
    val exerciseDay = mkt.spreadOptionExpiry(june)
    val T = exerciseDay.endOfDay.timeSince(md)
    val volume = 10.0(mkt.uom)
    val cso = new CommoditySpreadOption(mkt, june, K(mkt.priceUOM), volume, Put)
    val explanation = cso.explanation(env)
    assertEquals(explanation.name, "((SpreadOption-Put(Spread Price, Std Dev, K) * Volume) * Discount)")
    assertEquals(explanation.format(1), "((SpreadOption-Put((ICE WTI.JUN 2010 - IPE Brent.JUN 2010), 0.50 USD/bbl, 0.23 USD/bbl) * 10.00 bbl) * USD.28May2010)")
    val lastExplanation = "((SpreadOption-Put((101.00 USD/bbl - 111.00 USD/bbl), 0.50 USD/bbl, 0.23 USD/bbl) * 10.00 bbl) * 0.96)"
    assertEquals(explanation.format(2), lastExplanation)
    assertEquals(explanation.format(3), lastExplanation)
  }
}
