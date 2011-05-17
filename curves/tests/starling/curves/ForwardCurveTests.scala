package starling.curves

import starling.utils.StarlingTest
import starling.market.Market
import org.testng.annotations._
import org.testng.Assert._
import starling.daterange.Day
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.market.Market._
import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D =>DVector, DenseDoubleMatrix2D => DMatrix}
import starling.maths.RandomVariables

class ForwardCurveTests extends StarlingTest {

  @Test
  def testPerturbations{
    val marketDay = Day(2009, 1, 1)
    val nPrompts = 10
    val prompts = (0 until nPrompts).toList 
    val days = prompts.map{ i => (marketDay.containingMonth + i).lastDay}
    val prices = days.map{d => 100.0}
    val market = Market.LME_ALUMINIUM
    val curve = ForwardCurve.create(market, marketDay.endOfDay, days.zip(prices).toMap)
    val shifts = new DVector(nPrompts).assign(RandomVariables.standardUniform(12345)).toArray
    val shiftedCurve = curve.applyShifts(shifts)
    days.zip(shifts.toList).foreach{
      case (d, dP) =>
        assertEquals(shiftedCurve.price(d).value, curve.price(d).value + dP, 1e-9)
    }
  }
}
