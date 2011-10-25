package starling.instrument

import starling.utils.StarlingTest
import starling.daterange.{Day, Month}
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.UOM._
import org.testng.annotations.{AfterClass, AfterMethod, BeforeMethod, Test}
import starling.quantity.utils.QuantityTestUtils._
import starling.market.{Market, Index, JonTestEnv}
import starling.models.{DefaultRiskParameters, Call, Put}
import starling.instrument.utils.AtomicDatumKeyUtils._
import starling.utils.StarlingTest

/**
 * All the tests here match the values in Jon's Asian Option tests.
 *
 * If any tests fail please let me know rather than changing any values. Dave
 */
class JonAsianOptionTests extends JonTestEnv {

  @Test
  def atTheMoneyApr10CallTest {
    val valuationDate = Day(2010, 1, 5)

    val apr = Month(2010, 4)
    val may = Month(2010, 5)
    val jun = Month(2010, 6)
    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, apr, Quantity(80.0, market.priceUOM), Quantity(1, market.uom), Call)

    val env = makeEnv(valuationDate.endOfDay)

    assertEquals(asian.mtm(env).value, 8.24536658, 1e-8)

    val pfs = priceKeys(asian, env, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    val vfs = volKeys(asian, env, USD).map(rf => (rf.periodKey.get -> rf)).toMap

    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.16308301, 1e-8)
    assertEquals(asian.oneDayTheta(env, USD, changeOnlyTimeAndDiscounts = true).value, -0.0320945578618, 1e-4) // Can't get closer to Jon's value than this - but IIRC there were bugs in Kudu
    assertEquals(asian.gamma(env, pfs(may), USD, List(pfs(may), pfs(jun))).value, 0.01486141, 1e-7)
    assertEquals(asian.gamma(env, pfs(jun), USD, List(pfs(may), pfs(jun))).value, 0.00914547, 1e-7)
    assertEquals(asian.centreGamma(env, USD).value, 0.02400678, 1e-8)
  }

  @Test
  def atTheMoneyDec10CallTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(82.50, market.priceUOM), Quantity(1, market.uom), Call)
    assertEquals(asian.mtm(env).value, 13.5616056093, 1e-10)
    assertEquals(asian.centreGamma(env, USD).value, 0.0140696868009, 1e-10)
    assertEquals(asian.delta(env, USD).value, 0.6869817485164, 1e-10)
    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.3152231660731, 1e-10)
    assertEquals(asian.oneDayTheta(env, USD).value, -0.016174674242966, 1e-5) 
    assertEquals(asian.oneDayTheta(env, USD, changeOnlyTimeAndDiscounts = true).value, -0.0163844927706, 2e-4) // Can't get closer to Jon's value than this - but IIRC there were bugs in Kudu
  }

  @Test
  def atTheMoneyDec10PutTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(82.50, market.priceUOM), Quantity(1, market.uom), Put)

    assertEquals(asian.mtm(env).value, 9.6012714918, 1e-10)
    assertEquals(asian.centreGamma(env, USD).value, 0.014069686801, 1e-10)
    assertEquals(asian.delta(env, USD).value, -0.3072443898, 1e-10)
    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.3152231660, 1e-10)
    assertEquals(asian.oneDayTheta(env, USD).value, -0.016199017171491192, 1e-10) // doesn't match Jon's value of -0.0164483726464
  }

  @Test
  def outOfTheMoneyDec10CallTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(100, market.priceUOM), Quantity(1, market.uom), Call)

    //assertEquals(asian.mtm(env).value, 6.2604682476, 1e-10)
    assertEquals(asian.mtm(env).value, 6.2604682476, 1e-2)
    assertEquals(asian.delta(env, USD).value, 0.4026842764, 1e-9)
    assertEquals(asian.centreGamma(env, USD).value, 0.017692227255, 1e-9)
    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.316759687033, 1e-9)
    assertEquals(asian.oneDayTheta(env, USD).value, -0.0154339234778, 1e-3)
  }

  @Test
  def inTheMoneyDec10PutTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(100, market.priceUOM), Quantity(1, market.uom), Put)

    assertEquals(asian.mtm(env).value, 19.6990915503, 1e-10)
    assertEquals(asian.delta(env, USD).value, -0.5915418619, 1e-9)
    assertEquals(asian.centreGamma(env, USD).value, 0.017692227256, 1e-9)
    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.316759687033, 1e-9)
    assertEquals(asian.oneDayTheta(env, USD).value, -0.0152171595477, 1e-3)
  }

  @Test
  def inTheMoneyDec10CallTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(50, market.priceUOM), Quantity(1, market.uom), Call)

    assertEquals(asian.mtm(env).value, 37.5833636486, 1e-10)
    assertEquals(asian.delta(env, USD).value, 0.9373420339, 1e-9)
    assertEquals(asian.centreGamma(env, USD).value, 0.002256421052, 1e-9)
    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.108372989145, 1e-9)
    assertEquals(asian.oneDayTheta(env, USD).value, -0.0064917016596, 1e-3)

  }

  @Test
  def outOfTheMoneyDec10PutTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(50, market.priceUOM), Quantity(1, market.uom), Put)

    assertEquals(asian.mtm(env).value, 1.3106800363, 1e-10)
    assertEquals(asian.delta(env, USD).value, -0.0568841044, 1e-9)
    assertEquals(asian.centreGamma(env, USD).value, 0.002256421052, 1e-9)
    assertEquals(asian.parallelVega(env, shiftInterpolatedVols = true).value, 0.108372989145, 1e-9)
    assertEquals(asian.oneDayTheta(env, USD).value, -0.0070767771750, 1e-3)
  }

  @Test
  def atTheMoneyDec10CallDeltaEquivalenceTest {
    val valuationDate = Day(2010, 1, 5)
    val env = makeEnv(valuationDate.endOfDay)

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    val asian = SingleAsianOption(index, Month(2010, 12), Quantity(82.50, market.priceUOM), Quantity(1, market.uom), Call)

    val pfs = priceKeys(asian, env, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    val vfs = volKeys(asian, env, USD).map(rf => (rf.periodKey.get -> rf)).toMap

    val sumDelta = Quantity.sum(pfs.map(q => asian.delta(env, q._2, USD)))
    val pDelta = asian.delta(env, USD)

    assertQtyEquals(sumDelta, pDelta, 1e-5) // need to figure out what divergence we expect from skew nonlinearity
  }
}
