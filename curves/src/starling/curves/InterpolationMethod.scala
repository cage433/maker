package starling.curves

import starling.daterange._
import starling.quantity.Quantity
import starling.utils.UnboxedUnionTypes._


trait InterpolationMethod{
  def interpolate[QD: (Quantity |∨| Double)#λ](days : Array[Day], prices : Array[QD], day : Day) : QD
}

/** Interpolates a forward curve linearly. Currently extrapolates if interpoland is beyond
 * 	the last available day.
 * 	@todo - Find out if extrapolation should be switched off
 */
object LinearInterpolation extends InterpolationMethod{
  def interpolate[QD: (Quantity |∨| Double)#λ](days : Array[Day], prices : Array[QD], day : Day) : QD = {
  		val i_firstOnOrAfter = days.indexWhere(_ >= day)

  		i_firstOnOrAfter match {
  		  case -1 => prices.last
  		  case 0 => prices(0)
  		  case _	=> {
  		    val p0 = prices(i_firstOnOrAfter - 1)
  		    val p1 = prices(i_firstOnOrAfter)
  		    val x0 = 0
  		    val x1 = days(i_firstOnOrAfter).daysSinceInYears(days(i_firstOnOrAfter - 1))
  		    val x = day.daysSinceInYears(days(i_firstOnOrAfter - 1))

          (p0, p1) match {
            case (q0: Quantity, q1: Quantity) => assertPositive(q0 + (q1 - q0) * x / x1).asInstanceOf[QD]
            case (d0: Double, d1: Double) => assertPositive(d0 + (d1 - d0) * x / x1).asInstanceOf[QD]
          }
  		  }
  		}
  }

  private def assertPositive(q: Quantity): Quantity = { assert(q > 0, "Non positive price " + q); q}
  private def assertPositive(d: Double): Double = { assert(d > 0, "Non positive price " + d); d}
}

/**
 * Interpolates by returning the value of the first available price point on or
 * before the interpolated day. Currently extrapolates if interpoland is before
 * the first available day.
 * @todo - Find out if extrapolation should be switched off
 */
object InverseConstantInterpolation extends InterpolationMethod{
  def interpolate[QD: (Quantity |∨| Double)#λ](days : Array[Day], prices : Array[QD], day : Day) : QD = {
  		assert(prices.size > 0, "Can't interpolate without prices")
  		val i_firstStrictlyAfter = days.indexWhere(_ > day)

  		i_firstStrictlyAfter match {
  		  case -1 => prices.last
        case 0 => prices.head
  		  case _ => prices(i_firstStrictlyAfter - 1)
      }
	}
}