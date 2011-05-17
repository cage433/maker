package starling.curves

import starling.quantity.UOM._
import org.testng.Assert._
import starling.utils.QuantityTestUtils._
import org.testng.annotations.Test
import starling.market.{TestExpiryRules, Market}
import starling.quantity.{Percentage, Quantity}
import starling.daterange.{Spread, Day, Month}
import cern.colt.matrix.impl.DenseDoubleMatrix2D

class EnvironmentTests extends TestExpiryRules {


  @Test
  def testSimpleSpotFX = {
    val env = Environment(new UnitTestingAtomicEnvironment(Day(2011, 1, 1).endOfDay, {
      case _ => throw new Exception("not needed")
    }
    ))

    val oneDollar = Quantity(1, USD)
    val oneHundredCent = Quantity(100, US_CENT)
    assertEquals(oneDollar * env.spotFXRate(USD, USD), oneDollar)
    val cent = env.spotFXRate(US_CENT, USD)
    assertEquals(oneDollar * cent, oneHundredCent)
    assertEquals(oneHundredCent * env.spotFXRate(USD, US_CENT), oneDollar)
  }

  @Test
  def testPriceShifts = {
    val NymexWTI = Market.NYMEX_WTI
    val IceWTI = Market.ICE_WTI
    val IceBrent = Market.ICE_BRENT

    val Feb = Month(2011, 2)
    val Mar = Month(2011, 3)

    val priceUOM = USD / BBL
    val env = Environment(new UnitTestingAtomicEnvironment(Day(2011, 1, 1).endOfDay, {
      case ForwardPriceKey(NymexWTI, Feb, _) => Quantity(100, priceUOM)
      case ForwardPriceKey(NymexWTI, Mar, _) => Quantity(110, priceUOM)
      case ForwardPriceKey(IceWTI, Feb, _) => Quantity(101, priceUOM)
      case ForwardPriceKey(IceWTI, Mar, _) => Quantity(111, priceUOM)
      case ForwardPriceKey(IceBrent, _, _) => Quantity(115, priceUOM)
    }
    ))

    assertQtyEquals(env.forwardPrice(NymexWTI, Feb), Quantity(100, priceUOM))
    assertQtyEquals(env.forwardPrice(NymexWTI, Mar), Quantity(110, priceUOM))
    assertQtyEquals(env.forwardPrice(IceWTI, Feb), Quantity(101, priceUOM))
    assertQtyEquals(env.forwardPrice(IceWTI, Mar), Quantity(111, priceUOM))
    assertQtyEquals(env.forwardPrice(IceBrent, Mar), Quantity(115, priceUOM))

    assertEquals(NymexWTI.commodity, IceWTI.commodity)
    assertNotSame(NymexWTI.commodity, IceBrent.commodity)

    val shiftedFebWTI = env.shiftPrice(NymexWTI.commodity, Feb, Quantity(-5, priceUOM))
    assertQtyEquals(shiftedFebWTI.forwardPrice(NymexWTI, Feb), Quantity(95, priceUOM))
    assertQtyEquals(shiftedFebWTI.forwardPrice(NymexWTI, Mar), Quantity(110, priceUOM))
    assertQtyEquals(shiftedFebWTI.forwardPrice(IceWTI, Feb), Quantity(96, priceUOM))
    assertQtyEquals(shiftedFebWTI.forwardPrice(IceWTI, Mar), Quantity(111, priceUOM))
    assertQtyEquals(shiftedFebWTI.forwardPrice(IceBrent, Mar), Quantity(115, priceUOM))

    val shiftedParallelWTI = env.parallelShiftPrices(NymexWTI.commodity, Quantity(-5, priceUOM))
    assertQtyEquals(shiftedParallelWTI.forwardPrice(NymexWTI, Feb), Quantity(95, priceUOM))
    assertQtyEquals(shiftedParallelWTI.forwardPrice(NymexWTI, Mar), Quantity(105, priceUOM))
    assertQtyEquals(shiftedParallelWTI.forwardPrice(IceWTI, Feb), Quantity(96, priceUOM))
    assertQtyEquals(shiftedParallelWTI.forwardPrice(IceWTI, Mar), Quantity(106, priceUOM))
    assertQtyEquals(shiftedParallelWTI.forwardPrice(IceBrent, Mar), Quantity(115, priceUOM))

    val shiftedIceBrent = env.parallelShiftPrices(IceBrent, Quantity(-5, priceUOM))
    assertQtyEquals(shiftedIceBrent.forwardPrice(NymexWTI, Feb), Quantity(100, priceUOM))
    assertQtyEquals(shiftedIceBrent.forwardPrice(NymexWTI, Mar), Quantity(110, priceUOM))
    assertQtyEquals(shiftedIceBrent.forwardPrice(IceWTI, Feb), Quantity(101, priceUOM))
    assertQtyEquals(shiftedIceBrent.forwardPrice(IceWTI, Mar), Quantity(111, priceUOM))
    assertQtyEquals(shiftedIceBrent.forwardPrice(IceBrent, Mar), Quantity(110, priceUOM))
  }

  @Test
  def testVolShifts = {
    val NymexWTI = Market.NYMEX_WTI
    val IceWTI = Market.ICE_WTI
    val IceBrent = Market.ICE_BRENT

    val Feb = Month(2011, 2)
    val Mar = Month(2011, 3)

    val priceUOM = USD / BBL
    val env = Environment(new UnitTestingAtomicEnvironment(Day(2011, 1, 1).endOfDay, {
      case OilAtmVolAtomicDatumKey(NymexWTI, _, Feb, _) => Percentage(.2)
      case OilAtmVolAtomicDatumKey(NymexWTI, _, Mar, _) => Percentage(.25)
      case OilAtmVolAtomicDatumKey(IceWTI, _, Feb, _) => Percentage(.21)
      case OilAtmVolAtomicDatumKey(IceWTI, _, Mar, _) => Percentage(.26)
      case OilAtmVolAtomicDatumKey(IceBrent, _, _, _) => Percentage(.3)
    }
    ))

    assertEquals(env.atmImpliedVol(NymexWTI, Feb), Percentage(.2))
    assertEquals(env.atmImpliedVol(NymexWTI, Mar), Percentage(.25))
    assertEquals(env.atmImpliedVol(IceWTI, Feb), Percentage(.21))
    assertEquals(env.atmImpliedVol(IceWTI, Mar), Percentage(.26))
    assertEquals(env.atmImpliedVol(IceBrent, Mar), Percentage(.3))

    val shiftedFebWTI = env.shiftVol(NymexWTI.commodity, Feb, Percentage(-.05))
    assertEquals(shiftedFebWTI.atmImpliedVol(NymexWTI, Feb), Percentage(.15))
    assertEquals(shiftedFebWTI.atmImpliedVol(NymexWTI, Mar), Percentage(.25))
    assertEquals(shiftedFebWTI.atmImpliedVol(IceWTI, Feb), Percentage(.16))
    assertEquals(shiftedFebWTI.atmImpliedVol(IceWTI, Mar), Percentage(.26))
    assertEquals(shiftedFebWTI.atmImpliedVol(IceBrent, Mar), Percentage(.3))

    val shiftedWTI = env.parallelShiftVols(NymexWTI.commodity, Percentage(-.05))
    assertEquals(shiftedWTI.atmImpliedVol(NymexWTI, Feb), Percentage(.15))
    assertEquals(shiftedWTI.atmImpliedVol(NymexWTI, Mar), Percentage(.20))
    assertEquals(shiftedWTI.atmImpliedVol(IceWTI, Feb), Percentage(.16))
    assertEquals(shiftedWTI.atmImpliedVol(IceWTI, Mar), Percentage(.21))
    assertEquals(shiftedWTI.atmImpliedVol(IceBrent, Mar), Percentage(.30))

    val shiftedIceBrent = env.parallelShiftVols(IceBrent, Percentage(-.05))
    assertEquals(shiftedIceBrent.atmImpliedVol(NymexWTI, Feb), Percentage(.2))
    assertEquals(shiftedIceBrent.atmImpliedVol(NymexWTI, Mar), Percentage(.25))
    assertEquals(shiftedIceBrent.atmImpliedVol(IceWTI, Feb), Percentage(.21))
    assertEquals(shiftedIceBrent.atmImpliedVol(IceWTI, Mar), Percentage(.26))
    assertEquals(shiftedIceBrent.atmImpliedVol(IceBrent, Mar), Percentage(.25))
  }

  @Test
  def testStdDevShifts = {
    val NymexWTI = Market.NYMEX_WTI
    val IceWTI = Market.ICE_WTI
    val IceBrent = Market.ICE_BRENT

    val Feb = Month(2011, 2)
    val Mar = Month(2011, 3)
    val Apr = Month(2011, 4)
    val Spread1 = Spread(Feb, Mar)
    val Spread2 = Spread(Mar, Apr)

    val priceUOM = USD / BBL
    val env = Environment(new UnitTestingAtomicEnvironment(Day(2011, 1, 1).endOfDay, {
      case ForwardPriceKey(_, _, _) => Quantity(10, priceUOM)
      case SpreadAtmStdDevAtomicDatumKey(NymexWTI, Spread1, _) => Quantity(.2, priceUOM)
      case SpreadAtmStdDevAtomicDatumKey(NymexWTI, Spread2, _) => Quantity(.25, priceUOM)
      case SpreadAtmStdDevAtomicDatumKey(IceWTI, Spread1, _) => Quantity(.21, priceUOM)
      case SpreadAtmStdDevAtomicDatumKey(IceWTI, Spread2, _) => Quantity(.26, priceUOM)
      case SpreadAtmStdDevAtomicDatumKey(IceBrent, Spread1, _) => Quantity(.3, priceUOM)
      case SpreadSkewStdDevAtomicDatumKey(market, period) => {
        val matrix = new DenseDoubleMatrix2D(1, 2)
        matrix.set(0, 0, 0.0);
        matrix.set(0, 1, 5.0)
        matrix
      }
    }
    )).undiscounted

    assertEquals(env.spreadStdDev(NymexWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.2, priceUOM))
    assertEquals(env.spreadStdDev(NymexWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.25, priceUOM))
    assertEquals(env.spreadStdDev(IceWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.21, priceUOM))
    assertEquals(env.spreadStdDev(IceWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.26, priceUOM))
    assertEquals(env.spreadStdDev(IceBrent, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.3, priceUOM))

    val shiftedFebMarWTI = env.shiftSpreadStdDevs(NymexWTI.commodity, Spread1, Quantity(-.05, priceUOM))
    assertEquals(shiftedFebMarWTI.spreadStdDev(NymexWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.15, priceUOM))
    assertEquals(shiftedFebMarWTI.spreadStdDev(NymexWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.25, priceUOM))
    assertEquals(shiftedFebMarWTI.spreadStdDev(IceWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.16, priceUOM))
    assertEquals(shiftedFebMarWTI.spreadStdDev(IceWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.26, priceUOM))
    assertEquals(shiftedFebMarWTI.spreadStdDev(IceBrent, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.3, priceUOM))

    val shiftedWTI = env.parallelShiftSpreadStdDevs(NymexWTI.commodity, Quantity(-.05, priceUOM))
    assertEquals(shiftedWTI.spreadStdDev(NymexWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.15, priceUOM))
    assertEquals(shiftedWTI.spreadStdDev(NymexWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.20, priceUOM))
    assertEquals(shiftedWTI.spreadStdDev(IceWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.16, priceUOM))
    assertEquals(shiftedWTI.spreadStdDev(IceWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.21, priceUOM))
    assertEquals(shiftedWTI.spreadStdDev(IceBrent, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.3, priceUOM))

    val shiftedIceBrent = env.parallelShiftSpreadStdDevs(IceBrent, Quantity(-.05, priceUOM))
    assertEquals(shiftedIceBrent.spreadStdDev(NymexWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.2, priceUOM))
    assertEquals(shiftedIceBrent.spreadStdDev(NymexWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.25, priceUOM))
    assertEquals(shiftedIceBrent.spreadStdDev(IceWTI, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.21, priceUOM))
    assertEquals(shiftedIceBrent.spreadStdDev(IceWTI, Spread2, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.26, priceUOM))
    assertEquals(shiftedIceBrent.spreadStdDev(IceBrent, Spread1, Day(2011, 1, 3), Quantity(1, priceUOM)), Quantity(.25, priceUOM))

  }

}