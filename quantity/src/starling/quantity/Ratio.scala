package starling.quantity

import annotation.tailrec

object Ratio {
  @tailrec
  def gcd(a: Long, b: Long): Long = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }
}

case class Ratio(numerator: Long, denominator: Long) {

  import math.pow

  def gcd: Long = if (numerator == 0)
    1
  else {
    Ratio.gcd(numerator, denominator)
  }

  def reduce = if (numerator == 0)
    this
  else {
    val g = Ratio.gcd(numerator, denominator)
    Ratio(numerator / g, denominator / g)
  }

  def *(rhs: Long) = Ratio(numerator * rhs, denominator)

  def *(rhs: Ratio) = Ratio(numerator * rhs.numerator, denominator * rhs.denominator)

  def /(rhs: Ratio) = this * (rhs.inverse)

  def ^(power: Int) = {
    def positivePower(u: Ratio, p: Int) = Ratio(pow(u.numerator, p).asInstanceOf[Int], pow(u.denominator, p).asInstanceOf[Int])

    if (power >= 0) {
      positivePower(this, power)
    } else {
      positivePower(inverse, -power)
    }
  }

  def inverse = Ratio(denominator, numerator)

  def doubleValue = denominator.doubleValue / numerator.doubleValue

  def gcd(rhs: Ratio) = Ratio(Ratio.gcd(numerator, rhs.numerator), Ratio.gcd(denominator, rhs.denominator))

  def numeratorRatio = Ratio(numerator, 1)

  def denominatorRatio = Ratio(denominator, 1)
}