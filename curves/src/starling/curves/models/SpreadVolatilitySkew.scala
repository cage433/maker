package starling.curves.models

import starling.maths.LeastSquaresFit
import starling.models.{SpreadOptionCalculations, Call}
import scala.math._

class SpreadVolatilitySkew(val atmVol: Double, val atmVolWithoutShiftsIfPossible: Double, val r: Double, val T: Double, val putSkew: Double, val callSkew: Double,
                                val polyOrder: Int) {
	assert(T > 0.0, "T must be in the future, received " + T)

  val DF = exp(-r * T)
  val skewByDelta = Map[Double, Double](0.0 -> callSkew, DF -> putSkew)

	val atmDelta = 0.5 * DF
	val polynomialCoefficients = LeastSquaresFit.fitPolynomialCoefficientsWithZero(polyOrder, atmDelta, skewByDelta)
	val postFitVols = skewByDelta.keys.map(volatilityByDelta(_)).toList.toArray

	def getPolynomialCoefficients = Some(polynomialCoefficients)

	def volatilityByDelta(delta: Double): Double = {
		var value: Double = 0.0
		for (i <- 0 until polyOrder)
			value += polynomialCoefficients(i) * pow(delta - atmDelta, i + 1)

		atmVol + value
	}

  private def deltaByStrike(strike: Double, forwardPrice: Double): Double =
    new SpreadOptionCalculations(Call, forwardPrice, strike, atmVolWithoutShiftsIfPossible, r, T).analyticDelta

	def volatilityByStrike(strike: Double, forwardPrice: Double): Double = {
    val delta = deltaByStrike(strike, forwardPrice)
    volatilityByDelta(delta)
  }
}
