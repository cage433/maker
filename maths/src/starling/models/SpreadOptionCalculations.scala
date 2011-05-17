package starling.models

import starling.maths.RandomVariables._
import starling.utils.ImplicitConversions._
import math._
import cern.jet.random.Normal

/**
 * This could simply be in CalendarSpreadOption itself, but to easily put in Jon's unit tests it was simpler
 * to separate them
 */
class SpreadOptionCalculations(callPut : CallOrPut, F : Double, K : Double, annualisedStdDev : Double, zeroRate : Double, T : Double){
  require(annualisedStdDev >= 0.0, "Std dev cannot be negative, got " + annualisedStdDev)
  require(T >= 0.0, "Time cannot be negative, got " + T)
  val standardNormalInstance : Normal = standardNormal(12345)

  val stdDev = annualisedStdDev * sqrt(T)
  private val isWorthIntrinsic = stdDev == 0.0
  val (n1, _N1) = {
    if (isWorthIntrinsic && F >= K)
        (0.0, 1.0)
    else if (isWorthIntrinsic && F < K)
        (0.0, 0.0)
    else {
      val d1 = (F - K) / stdDev
      (standardNormalInstance.pdf(d1), standardNormalInstance.cdf(d1))
    }
  }
  val df = exp(-zeroRate * T)

  val undiscountedPrice = callPut match {
    case Call if isWorthIntrinsic => (F - K) ^+
    case Put if isWorthIntrinsic => (K - F) ^+
    case Call => stdDev * n1 + (F - K) * _N1
    case Put => stdDev * n1 + (F - K) * (_N1 - 1.0)
  }

  val price = undiscountedPrice * df

  val analyticUndiscountedDelta = callPut match {
    case Call => _N1
    case Put  => _N1 - 1.0
  }

  val analyticDelta = analyticUndiscountedDelta * df

  val analyticUndiscountedGamma = if (isWorthIntrinsic) 0.0 else n1 / stdDev
}
