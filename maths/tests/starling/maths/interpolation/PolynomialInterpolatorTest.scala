package starling.maths.interpolation

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import starling.utils.conversions.RichColtMatrices._
import org.testng.Assert._
import starling.quantity.utils.QuantityTestUtils._
import math.log

class PolynomialInterpolatorTest extends TestNGSuite {
  @Test
  def interpolate {
    var interp = new PolynomialInterpolator(4)
    var x = new DVector(10)
    var y = new DVector(10)

    for (i <- 0 until 10) {
      x(i) = i
      y(i) = log(i + 1)
    }

    var d = interp.interpolate(x, y, 4.5)
    assertQtyEquals(d, log(4.5 + 1), 1e-4)
  }

}
