package starling.maths

import starling.utils.CollectionUtils._
import math._

/**
 * A lognormal variable
 * NB: Note that the parameter sigma is NOT vol. For a forward price we would have
 *  mu = price
 *  sigma = vol * sqrt(T)
 */
case class Lognormal(mu : Double, val sigma : Double){

  val mean = exp(mu + 0.5 * sigma * sigma)
  val variance = ( (exp(sigma * sigma) - 1.0) * exp(2 * mu + sigma * sigma) ).abs
  val stdDev = sqrt(variance)

  def multiply(rhs : Lognormal, rho : Double) : Lognormal = {
    val vol = sqrt(
      sigma * sigma +
      2 * rho * sigma * rhs.sigma +
      rhs.sigma * rhs.sigma)
    Lognormal(mu + rhs.mu, vol)
  }
  def covariance (rhs : Lognormal, rho : Double) : Double = {
    multiply(rhs, rho).mean - mean * rhs.mean
  }
  def correlation(rhs : Lognormal, rho : Double) : Double = {
    covariance(rhs, rho) / stdDev / rhs.stdDev
  }

  def stDevOfSpread(rhs: Lognormal, rho: Double): Double = {
    val corr = correlation(rhs, rho)
    val spreadVariance = ( variance - 2.0 * corr * stdDev * rhs.stdDev + rhs.variance ).abs
    sqrt(spreadVariance)
  }
  def randomVariable(seed : Int) : RandomVariable = new RandomVariable(){
    val n = RandomVariables.normal(-0.5 * sigma * sigma, sigma, seed)

    def nextDouble = mean * exp(n.nextDouble)
  }

}

object Lognormal{
  def apply(price : Double, vol : Double, T : Double) : Lognormal = {
    val sigma = vol * sqrt(T)
    val mu = log(price) - 0.5 * sigma * sigma
    Lognormal(mu, sigma)
  }
}
object LognormalCalcs{
  /** Returns the variance of a lognormal price with known volatility and time
   */
  def variance(price : Double, vol : Double, time : Double) : Double = {
    val mu = log(price) - 0.5 * vol * vol * time
    val v = (exp(vol * vol * time) - 1.0) * exp(2 * mu + vol * vol * time)
    v.abs // Catch negative vars that can be caused by numerical roundoff
  }

  def stdDev(price : Double, vol : Double, time : Double) : Double = {
    math.sqrt(variance(price, vol, time))
  }

  /** Returns the volatility given the variance of a lognormal price with known time to expiry
   *
   */
  def volatility(variance : Double, price : Double, time : Double) : Double = {
    if (time == 0)
      0
    else
      sqrt(log(1 + variance / (price * price)) / time)
  }

  /**Determines the variance of the average of a process which represents observations of forward prices over time. The
    *  Assumes that the prices are perfectly correlated, although each day's forward price can be different. Also assumes
   *  that the curve has a single volatility, although it is straightforward to adapt the code to have different vols for
   *  each day.
   *
   * Let $X_1, \dots X_n$ be the observations of the lognormal process on days $d_1, \dots , d_n$, then the variance of $Y=\sum_ { i=1 } ^n X_i$ is given by
   * $$
   * \eqalign  {
   * { \rm Var } (Y) &= \sum_ { i=1 } ^n \sum_ { j=1 } ^n E(X_iX_j) - (\sum_ { i=1 } ^n E(X_i))^2 \cr
   * &= \sum_ { i=1 } ^n { \rm Var } (X_i) + 2 * (\sum_ { i=1 } ^n \sum_ { j>i } ^n E(X_iX_j)-E(X_i)E(X_j)) \cr
   * &= \sum_ { i=1 } ^n { \rm Var } (X_i) + 2 * \sum_ { i=1 } ^n  { \rm Var } (X_i)\sum_ { j>i } ^n E(X_j/X_i) \cr
   * &= \sum_ { i=1 } ^n { \rm Var } (X_i) + 2 * \sum_ { i=1 } ^n  { \rm Var } (X_i)\sum_ { j>i } ^n E(X_j)/E(X_i) \cr
   * }
   * $$
   * \vfill\bye
   *
   */
  def varianceOfAverage(prices : List[Double], times : List[Double], volatility : Double) : Double = {
    val variances = prices.zip(times).map{
      case (p, t) => variance(p, volatility, t)
    }
    val n = prices.size
    var sum = 0.0
    (0 until n).foreach{
      i =>
        sum += variances(i)
        (i + 1 until n).foreach{
          j =>
          sum += 2 * variances(i) * prices(j) / prices(i)
        }
    }
    sum / (n * n)
  }

  def turnbullWakemanVol(prices: List[Double], vols: List[Double], times: List[Double]): Double = {
    val averageVol = averageDoubleSeq(vols)
    val variance = varianceOfAverage(prices, times, averageVol)
    val averagePrice = averageDoubleSeq(prices)
    val vol = volatility(variance, averagePrice, times.last)
    if (vol.isNaN){
      throw new Exception("Vol is NaN, (prices, vols, times) " + (prices, vols, times))
    }
    vol
  }
}
