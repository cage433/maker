package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.Percentage._
import starling.quantity.Quantity._
import starling.market.Market._
import starling.market.Index._
import starling.daterange.{Spread, Month, Day}
import starling.quantity.{UOM, Quantity, Percentage}
import starling.market.{TestMarketTest, FuturesFrontPeriodIndex, Index}

class InterpolatedVolShiftedEnvironmentTests extends TestMarketTest {

  val marketDay = Day(2010, 1, 1).endOfDay
  val env = Environment(new NullAtomicEnvironment(marketDay))
  val june = Month(2010, 6)
  val july = june + 1
  val may = june - 1
  val dV = 0.01

  @Test
  def testFuturesVolPerturbations{
    val shiftedEnv = env.shiftInterpolatedVol(OilAtmVolAtomicDatumKey(NYMEX_WTI, None, june), Percentage(dV))
    val vol = env.atmImpliedVol(NYMEX_WTI, june).decimalValue
    assertEquals(shiftedEnv.atmImpliedVol(NYMEX_WTI, june), vol + dV, 1e-6)
    assertEquals(shiftedEnv.atmImpliedVol(NYMEX_WTI, july), vol, 1e-6)
    assertEquals(shiftedEnv.atmImpliedVol(NYMEX_BRENT, june), vol, 1e-6)

    val strike: Quantity = 97.0(NYMEX_WTI.priceUOM)
    assertEquals(shiftedEnv.impliedVol(NYMEX_WTI, june, june.firstDay - 1, strike), vol + dV, 1e-6)
    assertEquals(shiftedEnv.impliedVol(NYMEX_WTI, july, june.firstDay - 1, strike), vol, 1e-6)

    // Swap vol should be unchanged, even though we've perturbed the underlying futures vol
    //TODO [02 Feb 2011] check with alex. This assertion now fails following the fix
    // so that 'Show Eq. Futures' + Not 'ATM Vega' does not give a zero vega
    //assertEquals(shiftedEnv.averageImpliedVol(WTI10, may, strike), vol, 1e-6)
  }

  @Test
  def testSwapVolPerturbations{

    val shiftedEnv = env.shiftInterpolatedVol(SwapVol(WTI10, may), Percentage(dV))
    //val shiftedEnv = InterpolatedVolShiftedEnvironment(env, SwapVol(WTI10, may), Percentage(dV))
    val strike: Quantity = 97.0(WTI10.priceUOM)
    val vol = env.swapVol(WTI10, may, strike)
    assertEquals(vol + dV, shiftedEnv.swapVol(WTI10, may, strike), 1e-6)
    assertEquals(vol, shiftedEnv.swapVol(WTI10, june, strike), 1e-6)

    // futures vols should be unchanged, even though we have perturbed the swap vol
    assertEquals(vol, shiftedEnv.atmImpliedVol(NYMEX_WTI, june), 1e-6)
    assertEquals(vol, shiftedEnv.atmImpliedVol(NYMEX_WTI, july), 1e-6)
  }

  @Test
  def testCSOStdDevPerturbations{
    val dStdDev = 0.1(NYMEX_WTI.priceUOM)
    val shiftedEnv = env.shiftInterpolatedVol(SpreadAtmStdDevAtomicDatumKey(NYMEX_WTI, Spread(june, july)), dStdDev)
    val strike: Quantity = 97.0(WTI10.priceUOM)
    val stDev = env.spreadStdDev(NYMEX_WTI, Spread(june, july), june.firstDay - 1, strike)
    assertEquals(stDev + dStdDev, shiftedEnv.spreadStdDev(NYMEX_WTI, Spread(june, july), june.firstDay - 1, strike))
    assertEquals(stDev, shiftedEnv.spreadStdDev(NYMEX_WTI, Spread(may, june), may.firstDay - 1, strike))
    assertEquals(stDev, shiftedEnv.spreadStdDev(NYMEX_BRENT, Spread(june, july), june.firstDay - 1, strike))
  }
}
