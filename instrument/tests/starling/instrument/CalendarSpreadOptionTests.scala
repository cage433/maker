package starling.instrument

import starling.utils.StarlingTest
import starling.curves._
import starling.quantity.Quantity._
import org.testng.annotations.{DataProvider, Test}
import math._
import starling.models.{Call, Put}
import org.testng.Assert._
import starling.market.{JonTestEnv, Market}
import starling.quantity.{Quantity, Percentage}
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.daterange._
import starling.utils.QuantityTestUtils._
import org.scalatest.testng.TestNGSuite
import starling.maths.StandardNormal
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import cern.colt.matrix.impl.DenseDoubleMatrix1D

class CalendarSpreadOptionTests  extends JonTestEnv with TestNGSuite{

  @Test
  def testSpread {
    val md = Day(2010, 1, 14).endOfDay

    val market = Market.NYMEX_WTI

    val env = makeEnv(md)

    val may = Month(2010, 5)
    val jun = Month(2010, 6)
    val jul = Month(2010, 7)
    val aug = Month(2010, 8)

    val strike = Quantity(0, market.priceUOM)
    val volume = Quantity(1000, BBL)
    val option1 = new SingleCalendarSpreadOption(market, market.csoOptionExpiry(may / jun), may, jun, strike, volume, Call)
    val option2 = new SingleCalendarSpreadOption(market, market.csoOptionExpiry(jun / jul), jun, jul, strike, volume, Call)
    val option3 = new SingleCalendarSpreadOption(market, market.csoOptionExpiry(jul / aug), jul, aug, strike, volume, Call)
    val mtm1 = option1.mtm(env)
    val mtm2 = option2.mtm(env)
    val mtm3 = option3.mtm(env)

    val start = SpreadPeriod(may, jun)
    val end = SpreadPeriod(jul, aug)
    val cso = new CalendarSpreadOption(market, StripPeriod(start, end), strike, volume, Call)

    assertEquals(cso.asUtpPortfolio(Day(2009, 1, 1)).portfolio, List(option1, option2, option3).map(o => (o.copy(volume = Quantity(1, BBL)) -> o.volume.value)).toMap)
    assertQtyEquals(cso.asUtpPortfolio(Day(2009, 1, 1)).mtm(env), mtm1 + mtm2 + mtm3)
  }

  @Test
  def testDeltaAndGamma{
    val md = Day(2010, 1, 14).endOfDay
    val june = Month(2010, 6)
    val july = Month(2010, 7)
    val mkt = Market.NYMEX_WTI
    val stdDev = 0.5
    val FJune = 101.0
    val FJuly = 100.0
    val K = 0.2333
    val env = Environment(
      new UnitTestingAtomicEnvironment(
        md,
        {key =>
          key match {
            case ForwardPriceKey(_, `june`, _) => FJune(mkt.priceUOM)
            case ForwardPriceKey(_, `july`, _) => FJuly(mkt.priceUOM)
            case _ : DiscountRateKey => 1.0
            case _ : SpreadAtmStdDevAtomicDatumKey => stdDev(mkt.priceUOM)
            case _ : SpreadSkewStdDevAtomicDatumKey => new DenseDoubleMatrix1D(Array(0.0, 0.0)).asRowMatrix
          }
        }
      )
    )
    val exerciseDay = june.firstDay - 1
    val T = exerciseDay.endOfDay.timeSince(md)
    val volume = 10.0(mkt.uom)
    val cso = new SingleCalendarSpreadOption(mkt, exerciseDay, june, july, K(mkt.priceUOM), volume, Put)
    val gamma = cso.gamma(env, FuturesSpreadPrice(mkt, june / july), USD, List(FuturesSpreadPrice(mkt, june / july)), multiple = 1e-3).value
    val delta = cso.firstOrderDerivative(env, FuturesSpreadPrice(mkt, june / july), USD, multiple = 1e-3).value
    val scaledVol = stdDev * sqrt(T)
    val spread = FJune - FJuly
    val d1 = (spread - K) / scaledVol
    val n1 = StandardNormal.pdf(d1)
    val N1 = StandardNormal.cdf(d1)
    val expectedDelta = volume.value * (N1 - 1)
    assertEquals(delta, expectedDelta, 1e-4)
    val expectedGamma = volume.value * n1 / scaledVol
    assertEquals(gamma, expectedGamma, 1e-4)
  }
}
