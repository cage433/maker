package starling.models

import starling.daterange.Day
import math._
import starling.daterange.DayAndTime

trait TimeStepBuilder {
  def times(marketDay: DayAndTime, exerciseDay: Day): Array[Double]
}

case class DayTimesBuilder(dayStep: Int = 1) extends TimeStepBuilder {
  def times(marketDay: DayAndTime, exerciseDay: Day): Array[Double] = {
    var days = List(exerciseDay.endOfDay)

    while (days.head - dayStep > marketDay)
      days = (days.head - dayStep) :: days

    while (days.head - 1 > marketDay)
      days = (days.head - 1) :: days

    if (days.head > marketDay)
      days = marketDay :: days

    days.map(_.timeSince(marketDay)).toArray
  }
}

class FixedTimesBuilder(nTimes: Int) extends TimeStepBuilder {
  def times(T: Double): Array[Double] = {
    val dt = T / (nTimes - 1)
    (0 until nTimes).toArray.map(_ * dt)

  }

  def times(marketDay: DayAndTime, exerciseDay: Day): Array[Double] = {
    times(exerciseDay.endOfDay.timeSince(marketDay))
  }
}


/**
 * Determines the shape of the grid on which finite difference valuation is done
 */
case class EnvironmentParameters(
  swapRoundingOK: Boolean = true,
  precision: Double = 1e-5,
  width: Int = 50,
  timeStepBuilder: TimeStepBuilder = new DayTimesBuilder(7)
  ) {
  /**
   *  The log(price) grid width
   */
  def dz(vol: Double, T: Double): Double = {
    val dxConfidence = vol * sqrt(T) * sqrt(log(1 / precision)) + 0.5 * vol * vol * T
    2 * dxConfidence / width
  }
}

/**
 *  For valuation, and VaR in particular, a relatively coarse grid gives accurate
 * enough results while also being quick enough
 */
object DefaultValuationParameters extends EnvironmentParameters()

/**
 * For greeks it is necessary to use a much finer grid to be able to do accurate
 * numeric differentiation. Slower of course, but calculating all the greeks for a trade
 * will involve revaluing perhaps tens of times, compared to thousands for VaR 
 */
object DefaultRiskParameters extends EnvironmentParameters(
  swapRoundingOK = false,
  width = 200,
  timeStepBuilder = new DayTimesBuilder(1)
  )
