package starling.utils.conversions

import java.lang.Math
import java.text.DecimalFormat
import starling.utils.MathUtil
import java.math.RoundingMode


trait RichDouble {
  implicit def DoubleToRichDouble(d : Double) = new RichDouble(d)

  class RichDouble(d : Double){
//    def fmt(formatString : String) : String = {
//      new java.text.DecimalFormat(formatString).format(d)
//    }
//    def fmt() : String = fmt("0.00")
//    def fmt(n : Int) : String = fmt("0." + "0" * n)
    def ^+ = Math.max(d, 0)

    def format(formatString: String, addSpace: Boolean = true) = {
      val decimalFormat = new DecimalFormat(formatString)
      decimalFormat.setRoundingMode(RoundingMode.HALF_UP)
      if (d < 0.0) {
        "(" + decimalFormat.format(d.abs) + ")"
      } else {
        decimalFormat.format(d) + (if(addSpace) " " else "")
      }
    }

    /**
     * If === zero then return Epsilon, otherwise this
     */
    def nonZero = if (d == 0.0) MathUtil.EPSILON else d
    def isAlmostZero = d.abs < MathUtil.EPSILON
  }

  object DoubleParse {
    def unapply(text: String): Option[Double] = try { Some(text.toDouble) } catch { case _ => None }
  }
}