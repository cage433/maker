package starling.maths


import scala.collection._
import starling.quantity.Percentage


object SplineInterpolator {
  def interpolate(xs: Array[Double], ys: Array[Percentage], x: Double): Percentage = Percentage(interpolate(xs, ys.map(_.decimalValue), x))

  def interpolate(xs: Array[Double], ys: Array[Double], x: Double): Double = {
    val sp = spline(xs, ys, 1e+40, 1e+40)
    splint(xs, ys, sp, x)
  }

  def spline(x: Array[Double], y: Array[Double], yp1: Double, ypn: Double) = {
    assert(x.length == y.length, "Need to have both coordinates")
    assert(x.length > 0, "Empty array")

    val n = x.length
    val y2 = new Array[Double](n)
    val u = new Array[Double](n - 1)

    if (yp1 > 1E+30) {
      y2(0) = 0.0
      u(0) = 0.0
    } else {
      y2(0) = -0.5
      u(0) = (3.0 / (x(1) - x(0))) * ((y(1) - y(0)) / (x(1) - x(0)) - yp1)
    }

    var sig = 0.0
    var p = 0.0

    // While loop prevents Boxing
    var i = 1
    while (i <= n - 2){
//    for (i <- 1 to n - 2) {
      sig = (x(i) - x(i - 1)) / (x(i + 1) - x(i - 1))
      p = sig * y2(i - 1) + 2.0
      y2(i) = (sig - 1.0) / p
      u(i) = (y(i + 1) - y(i)) / (x(i + 1) - x(i)) - (y(i) - y(i - 1)) / (x(i) - x(i - 1))
      u(i) = (6.0 * u(i) / (x(i + 1) - x(i - 1)) - sig * u(i - 1)) / p
      i += 1
    }

    var qn = 0.0
    var un = 0.0

    if (ypn > 9.9E+29) {
      qn = 0.0
      un = 0.0
    } else {

      qn = 0.5
      un = (3.0 / (x(n - 1) - x(n - 2))) * (ypn - (y(n - 1) - y(n - 2)) / (x(n - 1) - x(n - 2)))
    }

    y2(n - 1) = (un - qn * u(n - 2)) / (qn * y2(n - 2) + 1.0)

    // While loop prevents Boxing
        i = n - 2
    while (i >= 0){
//    for (i <- (0 to n - 2).reverse)
      y2(i) = y2(i) * y2(i + 1) + u(i)
      i -= 1
    }

    y2
  }

  def splint(xa: Array[Double], ya: Array[Double], y2a: Array[Double], x: Double) = {
    var klo = 0
    var khi = xa.length - 1

    while (khi - klo > 1) {
      val k: Int = (khi + klo) / 2
      if (xa(k) > x) khi = k else klo = k
    }

    val h = xa(khi) - xa(klo)
    if (h == 0.0) throw new Exception("Bad xa input to routine splint")

    val a = (xa(khi) - x) / h
    val b = (x - xa(klo)) / h

    a * ya(klo) + b * ya(khi) + ((a * a * a - a) * y2a(klo) + (b * b * b - b) * y2a(khi)) * h * h / 6.0
  }
}
