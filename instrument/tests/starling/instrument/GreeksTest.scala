package starling.instrument

import org.testng.annotations._
import org.testng.Assert._

import starling.quantity.UOM._
import starling.utils.ScalaTestUtils._
import starling.utils.QuantityTestUtils._
import starling.curves._
import starling.quantity.{Percentage, Quantity}
import starling.daterange.{Month, Day}
import starling.market.{JonTestEnv, TestExpiryRules, FuturesMarket, Market}
import starling.models.{Put, Call}
import starling.models._
import starling.utils.AtomicDatumKeyUtils._

class GreeksTest extends JonTestEnv {

  @Test
  def testCurveKeyForDelta {
    val md = Day(2010, 1, 1).endOfDay
    val env = makeEnv(md)
    val market = Market.NYMEX_WTI
    val month = Month(2010, 5)
    val p = env.forwardPrice(market, month)
    val option = new FuturesOption(market, market.optionExpiry(month), month, p, Quantity(1000, BBL), Call, American)

    val pfs = priceKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    val vfs = volKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap

    val delta1 = option.firstOrderDerivative(env, PriceDifferentiable(market, month), USD)
    val delta2 = option.delta(env, pfs(month), USD)
    assertQtyEquals(delta1, delta2)
  }

  @Test
  def testCurveKeyForVega {
    val md = Day(2010, 1, 1).endOfDay
    val env = makeEnv(md)
    val market = Market.NYMEX_WTI
    val month = Month(2010, 5)
    val p = env.forwardPrice(market, month)
    val option = new FuturesOption(market, market.optionExpiry(month), month, p, Quantity(1000, BBL), Call, American)

    val pfs = priceKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    val vfs = volKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap

    val vega1 = option.firstOrderDerivative(env, OilAtmVolAtomicDatumKey(market, None, month, true), USD)
    val vega2 = option.vega(env, vfs(month))
    assertQtyEquals(vega1 / 100.0, vega2)
  }

  @Test
  def testCurveKeyForVomma {
    val md = Day(2010, 1, 1).endOfDay
    val env = makeEnv(md)
    val market = Market.NYMEX_WTI
    val month = Month(2010, 5)
    val p = env.forwardPrice(market, month)
    val option = new FuturesOption(market, market.optionExpiry(month), month, p, Quantity(1000, BBL), Call, American)
    val vomma1 = option.secondOrderDerivative(env, OilAtmVolAtomicDatumKey(market, None, month, true), USD)

    val pfs = priceKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    val vfs = volKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    
    val vomma2 = option.vomma(env, vfs(month), List(vfs(month)))
    assertQtyEquals(vomma1 / (100.0 * 100.0), vomma2)
  }

  @Test
  def testBleed {
    val md = Day(2010, 4, 12).endOfDay
    val env = makeEnv(md)
    val market = Market.NYMEX_WTI
    val month = Month(2010, 5)
    val p = env.forwardPrice(market, month) - Quantity(-1, USD/BBL)
    val option = new FuturesOption(market, market.optionExpiry(month), month, p, Quantity(1000, BBL), Call, European)

    val pfs = priceKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap
    val vfs = volKeys(option, env.marketDay, USD).map(rf => (rf.periodKey.get -> rf)).toMap

    val deltaBleed = option.deltaBleed(env, pfs(month), md + 1, USD)
    // a little out of the money and close to expiry means delta is decreasing
    assertQtyEquals(deltaBleed, Quantity(-38.22, BBL), 1e-2)

    val gammaBleed = option.gammaBleed(env, pfs(month), md + 1, USD, List(pfs(month)))
    // a little out of the money and close to expiry means gamma is increasing. if we're very
    // close to expiry and near the money gamma will be huge.
    assertQtyEquals(gammaBleed, Quantity(28.67, BBL*BBL/USD), 1e-2)
  }
}
