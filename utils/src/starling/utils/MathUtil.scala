package starling.utils

import math.{pow, round}

object MathUtil {
  val EPSILON = 1e-6

  def roundToNdp(x: Double, n: Int): Double = {
    if (java.lang.Double.isNaN(x) || java.lang.Double.isInfinite(x)) {
      x
    } else {
      var mult: Double = pow(10.0, n)
      return (round(x * mult).asInstanceOf[Double]) / mult
    }
  }

  def roundTo2dp(x: Double): Double = {
    return roundToNdp(x, 2)
  }
}
