package starling.quantity

import math.pow

trait RatioT[R <: RatioT[R]] extends Ordered[R] {
  def *(rhs : R) : R
  def /(rhs : R) : R
  def ^(power : Int) : R
  def inverse : R

  def *(rhs : Long) : R
  def reduce : R
  def gcd(rhs : R) : R
}

object Ratio {
  def gcd(a : Long, b : Long): Long = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }
}

case class Ratio(numerator : Long, denominator : Long) extends RatioT[Ratio] {
  def compare(rhs: Ratio) = asString.compareTo(rhs.asString)

  def reduce = if (numerator == 0)
    this
  else {
    val g = Ratio.gcd(numerator, denominator)
    Ratio(numerator / g, denominator / g)
  }

  def *(rhs : Long) = Ratio(numerator * rhs, denominator).reduce
  def *(rhs : Ratio) = Ratio(numerator * rhs.numerator, denominator * rhs.denominator).reduce
  def /(rhs : Ratio) = this * (rhs.inverse)

  def ^(power : Int) = {
    def positivePower(u : Ratio, p : Int) = Ratio(pow(u.numerator, p).asInstanceOf[Long], pow(u.denominator, p).asInstanceOf[Long])

    if (power >= 0) {
      positivePower(this, power)
    } else {
      positivePower(inverse, -power)
    }
  }

  def inverse = Ratio(denominator, numerator)
  def doubleValue = denominator.doubleValue / numerator.doubleValue

  def gcd(rhs : Ratio) = Ratio(Ratio.gcd(numerator, rhs.numerator), Ratio.gcd(denominator, rhs.denominator))

  def numeratorRatio = Ratio(numerator, 1)
  def denominatorRatio = Ratio(denominator, 1)

  lazy val asString : String = reduce.toString
}