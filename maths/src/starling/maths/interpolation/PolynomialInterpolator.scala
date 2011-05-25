package starling.maths.interpolation

import cern.colt.matrix.{DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import starling.utils.conversions.RichColtMatrices._
import starling.utils.CollectionUtils._

class PolynomialInterpolator(val degree: Int = PolynomialInterpolator.CUBIC) {
  def interpolate(x: Vector, y: Vector, x0: Double): Double = {
    if (x.size == 1) {
      y(0)
    } else if (x.size <= degree) {
      new PolynomialInterpolator(x.size - 1).interpolate(x, y, x0)
    } else {
      var start = startOfRange(x, x0)
      var z = 0.0
      var value = 0.0
      for (i <- start to start + degree) {
        z = y(i)
        for (j <- start to start + degree) if (j != i) {
          z *= (x0 - x(j)) / (x(i) - x(j))
        }
        value += z
      }
      value
    }
  }

  /**
   * Polynomial interpolation is done on a sub-grid if the x-axis
   * this function returns the index of the start of the sub-grid
   */
  def startOfRange(x: Vector, x0: Double): Int = {
    var startOfRange: Int = 0
    if (degree % 2 == 0) {
      startOfRange = findNearest(x.toArray.toList, x0) - degree / 2
    } else {
      startOfRange = firstGtr(x.toArray, x0) - (degree + 1) / 2
    }
    if (startOfRange < 0) {
      startOfRange = 0
    }
    if (startOfRange > x.size - degree - 1) {
      startOfRange = x.size - degree - 1
    }
    startOfRange
  }
}
object PolynomialInterpolator {
  val LINEAR = 1
  val QUADRATIC = 2
  val CUBIC = 3
  val QUARTIC = 4
}