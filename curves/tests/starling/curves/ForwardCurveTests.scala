package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._
import starling.daterange.Day._
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.quantity.UOM._
import starling.market.Market._
import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.maths.RandomVariables
import collection.immutable.Map
import starling.quantity.utils.QuantityTestUtils._
import starling.market.{Index, Market}
import starling.daterange.{Month, DateRange, Day}

class ForwardCurveTests extends StarlingTest {

  @Test
  def testPerturbations {
    val marketDay = Day(2009, 1, 1)
    val nPrompts = 10
    val prompts = (0 until nPrompts).toList
    val days = prompts.map {
      i => (marketDay.containingMonth + i).lastDay
    }
    val prices = days.map {
      d => 100.0
    }
    val market = Market.LME_ALUMINIUM
    val curve = ForwardCurve.create(market, marketDay.endOfDay, days.zip(prices).toMap)
    val shifts = new DVector(nPrompts).assign(RandomVariables.standardUniform(12345)).toArray
    val shiftedCurve = curve.applyShifts(shifts)
    days.zip(shifts.toList).foreach {
      case (d, dP) =>
        assertEquals(shiftedCurve.price(d).value, curve.price(d).value + dP, 1e-9)
    }
  }

  @Test
  def testLMEInterpolation {
    val marketDay = Day(2009, 1, 1)
    val market = Market.LME_ALUMINIUM

    val prices: Map[DateRange, Quantity] = Map(
      (15 Feb 2010) -> 15(USD / MT),
      (1 Feb 2010) -> 1(USD / MT),
      (21 Feb 2010) -> 21(USD / MT)
    )

    val curve = ForwardCurve(market, marketDay.endOfDay, prices)

    assertQtyEquals(curve.price(15 Feb 2010), 15(USD / MT))
    assertQtyEquals(curve.price(14 Feb 2010), 14(USD / MT))
    assertQtyEquals(curve.price(20 Feb 2010), 20(USD / MT))
    assertQtyEquals(curve.price(2 Feb 2010), 2(USD / MT))
  }

  @Test
  def testNaphthaInterpolation {
    val marketDay = Day(2009, 1, 1)
    val market = Index.NAPHTHA_CFR_JAPAN

    val prices: Map[DateRange, Quantity] = Map(
      (16 Feb 2010) -> 2(USD / MT),
      (1 Feb 2010) -> 1(USD / MT),
      (1 Mar 2010) -> 3(USD / MT)
    )

    val curve = ForwardCurve(market, marketDay.endOfDay, prices)

    assertQtyEquals(curve.price(1 Feb 2010), 1(USD / MT))
    assertQtyEquals(curve.price(2 Feb 2010), 1(USD / MT))
    assertQtyEquals(curve.price(14 Feb 2010), 1(USD / MT))
    assertQtyEquals(curve.price(15 Feb 2010), 1(USD / MT))
    assertQtyEquals(curve.price(16 Feb 2010), 2(USD / MT))
    assertQtyEquals(curve.price(28 Feb 2010), 2(USD / MT))
    assertQtyEquals(curve.price(5 Mar 2010), 3(USD / MT))
  }

  @Test
  def testWTI_NO_Interpolation {
    val marketDay = Day(2009, 1, 1)
    val market = Market.NYMEX_WTI

    val prices: Map[DateRange, Quantity] = Map(
      Month(2011, 2) -> 2(USD / BBL),
      Month(2011, 1) -> 1(USD / BBL),
      Month(2011, 3) -> 3(USD / BBL)
    )

    val curve = ForwardCurve(market, marketDay.endOfDay, prices)


    try {
      val p = curve.price(1 Feb 2010)
      assertTrue(false, "Shouldn't have succeeded" + p)
    }
    catch {
      case e => assertTrue(e.isInstanceOf[MissingPriceException])
    }

    try {
      val p = curve.price(Month(2011, 4))
      assertTrue(false, "Shouldn't have succeeded" + p)
    }
    catch {
      case e => assertTrue(e.isInstanceOf[MissingPriceException])
    }

    try {
      val p = curve.price(Month(2010, 12))
      assertTrue(false, "Shouldn't have succeeded" + p)
    }
    catch {
      case e => assertTrue(e.isInstanceOf[MissingPriceException])
    }

    assertQtyEquals(curve.price(Month(2011, 1)), 1(USD / BBL))
    assertQtyEquals(curve.price(Month(2011, 2)), 2(USD / BBL))
    assertQtyEquals(curve.price(Month(2011, 3)), 3(USD / BBL))
  }
}
