package starling.models

import starling.utils.CollectionUtils._
import scala.math._
import starling.maths.{BrentSolver, RandomVariables}
import starling.quantity.{Percentage, Quantity}

class Curran(val callOrPut: CallOrPut, val S: Double, val K: Double, val vol: Double, val Ti: Array[Double],
             val T: Double, val runningAverage: Double, val fixedDays: Int) {
  assert(vol >= 0.0, "Vol is invalid")

  override def toString() = {
    "Call/Put " + callOrPut + 
    "\nS        " + S + 
    "\nK        " + K + 
    "\nvol      " + vol +
    "\nT        " + T + 
    "\nave      " + runningAverage + 
    "\nfixed    " + fixedDays + "\n\n"
  }

  private val standardNormal = RandomVariables.standardNormal(1234)
  private val unpricedDays = Ti.length
  private val totalDays = unpricedDays + fixedDays

  def discountedValue(zeroRate : Double) = undiscountedValue * exp(- zeroRate * T)
  
  lazy val undiscountedValue: Double = if (unpricedDays == 0 || vol * T == 0) {
    val averageS = (S * Ti.size + runningAverage * fixedDays) / (Ti.size + fixedDays)
    callOrPut.intrinsic(K)(averageS)
  } else {
    val adjustedStrike = (totalDays * K - fixedDays * runningAverage) / unpricedDays

    val dailyMeans = Ti.map(log(S) + (- vol * vol / 2.0) * _)
    val dailyVols = Ti.map(vol * sqrt(_))

    // TODO [12 Oct 2010] Figure out why implicit conversion is not working
    val meanOfLogGeometricAverage = averageDoubleSeq(dailyMeans)

    val volSquaredOverUnpricedDays = (vol * vol) / unpricedDays

    // TODO [12 Oct 2010] Express this in a functional style
    var sum = 0.0
    for (i <- 1 to unpricedDays) {
      for (j <- 1 to i) sum += Ti(j - 1)
      sum += (unpricedDays - i) * Ti(i - 1)
    }

    val volOfLogGeometricAverage = (vol / unpricedDays) * sqrt(sum)

    // TODO [12 Oct 2010] and this
    var covarianceXXi = new Array[Double](unpricedDays)
    for (i <- 0 until unpricedDays) {
      covarianceXXi(i) = 0.0
      for (j <- 0 to i) covarianceXXi(i) += Ti(j)
      covarianceXXi(i) += (unpricedDays - i - 1) * Ti(i)
      covarianceXXi(i) *= volSquaredOverUnpricedDays
    }

    val conditionExpectationFunction = (x: Double) =>
      averageDoubleSeq(List.range(0, unpricedDays).map(i => exp(
                                                               dailyMeans(i)
                                                                       + (covarianceXXi(i) / (volOfLogGeometricAverage * volOfLogGeometricAverage)) * (x - meanOfLogGeometricAverage)
                                                                       + 0.5 * (dailyVols(i) * dailyVols(i) - covarianceXXi(i) * covarianceXXi(i) / (volOfLogGeometricAverage * volOfLogGeometricAverage))
                                                               ))) - adjustedStrike

    val X = if (conditionExpectationFunction(0.0) > 0.0)
      0.0
    else if (conditionExpectationFunction(log(adjustedStrike)) < 0.0)
      log(adjustedStrike)
    else
      BrentSolver.solve(conditionExpectationFunction, 0.0, log(adjustedStrike))

    val callValueFirstTerm = averageDoubleSeq(List.range(0, unpricedDays).map(i => exp(dailyMeans(i) + dailyVols(i) * dailyVols(i) / 2)
            * standardNormal.cdf((meanOfLogGeometricAverage - X + covarianceXXi(i)) / volOfLogGeometricAverage)
                                                                             ))

    val callValueSecondTerm = adjustedStrike * standardNormal.cdf((meanOfLogGeometricAverage - X) / volOfLogGeometricAverage)

    val callValue = (1.0 * unpricedDays / totalDays) * (callValueFirstTerm - callValueSecondTerm)

    callOrPut match {
      case Call => callValue
      case Put => callValue + (1.0 * unpricedDays / totalDays) * (adjustedStrike - S)
      case Straddle => 2 * callValue + (1.0 * unpricedDays / totalDays) * (adjustedStrike - S)
    }
  }

  lazy val value = undiscountedValue
}

object Curran {
  /**
   * @param callOrPut
   * @param prices Map of times (since valuation date) to prices.
   * @param K strike
   * @param r interest rate
   * @param vol
   */
  def apply(callOrPut: CallOrPut, prices: Map[Double, Quantity], K: Quantity, vol: Percentage):Curran = {
    val allTi = prices.keys.toArray.sorted
    val Ti = allTi.filter(_ > 0.0)
    val T = allTi.last

    val S = Quantity.average(Ti.map(prices))

    val daysInPast = allTi.filter(_ <= 0.0)
    val fixedDays = daysInPast.length

    val fixings = daysInPast.map(prices)
    val runningAverage = if(fixings.isEmpty) Quantity(0.0, S.uom) else Quantity.average(fixings)

    assert(S.uom == runningAverage.uom)
    assert(K.uom == runningAverage.uom)
    val curran = new Curran(callOrPut, S.value, K.value, vol.value, Ti, T, runningAverage.value, fixedDays)
    curran
  }
}
