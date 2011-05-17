package starling.varcalculator

import cern.colt.matrix.DoubleFactory2D
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import cern.colt.matrix.{DoubleFactory2D, DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import starling.quantity.Quantity
import starling.utils.conversions.RichColtMatrices._
import starling.curves.ObservationDay
import starling.market.Market
import starling.daterange.{Month, DayAndTime, Day}

/** Calculates historic vols and correlations from the price history, also holds
 *  the most recent prices.
 */
case class RiskFactorStatistics(priceHistory : RiskFactorPriceHistory){
  private val snapshotPairs : List[(ObservationDay, ObservationDay)] = priceHistory.observationDays.zip(priceHistory.observationDays.tail)
  val riskFactors : List[VaRRiskFactor] = priceHistory.riskFactors
  val prices : Map[VaRRiskFactor, Quantity] = priceHistory.latestPrices
  lazy val pricesVector = new DVector(riskFactors.map(priceHistory.latestPrices(_).value).toArray)
  lazy val volsVector = new DVector(riskFactors.map(historicVols(_)).toArray)
  lazy val riskFactorUOMS = priceHistory.riskFactorUOMS

  /**
   *  The log returns are in the order most recent to oldest. This matters for exponentially weighted vols only,
   *  correlations are unaffected
   */
  private val logReturns : Map[VaRRiskFactor, Vector] = {
    var returns = Map.empty[VaRRiskFactor, Vector]
    for (rf <- riskFactors){
      val v = new DVector(snapshotPairs.size)
      snapshotPairs.reverse.zipWithIndex.foreach{
        case((snp1, snp2), i) =>
          val p1 = priceHistory.prices((rf, snp1))._2.value
          val p2 = priceHistory.prices((rf, snp2))._1.value
          val T = snp2.day.endOfDay.timeSince(snp1.day.endOfDay)
          v(i) = math.log(p2 / p1) / math.sqrt(T)
          if (v(i).isNaN)
            throw new IllegalStateException("Bad data, prices " + p1 + ", " + p2 + ", time " + T)
      }
      returns += rf -> v
    }
    returns
  }

  lazy val rhoMatrix = {
    val N = riskFactors.size
    val m = DoubleFactory2D.dense.identity(N)
    for (
      i <- 0 until N;
      j <- i + 1 until N
    ) {
      val v_i = logReturns(riskFactors(i))
      val v_j = logReturns(riskFactors(j))
      val rho = v_i.correlation(v_j)
      m(i, j) = rho
      m(j, i) = rho
    }
    m
  }

  lazy val historicVols : Map[VaRRiskFactor, Double] = {
    var vols = Map.empty[VaRRiskFactor, Double]
    for (rf <- riskFactors){
      var vol = logReturns(rf).expStdDev(0.94)
      if (vol.isNaN)
        throw new IllegalStateException("Bad vol, log returns are " + logReturns(rf))
      rf match {
      /**
       * The front Freight month price is actually this month, some will have fixed, and so its
       * vol will decrease through the month. As 'vol' is an average of observations across
       * all days in the front month, we scale it to take account of the time left.
       * Roughly it will be double the average at the beginning of the month, decreasing to
       * nothing at the end.
       * NB - I've gone for an approximate solution - no taking account of trading days, time of day etc.
       * It's only VaR.
       */
        case ForwardPriceRiskFactor(mkt : Market.BalticFuturesMarket, 0, 0) => {
          vol = vol * RiskFactorStatistics.freightFrontPeriodScaling(mkt, priceHistory.marketDay)
        }
        case _ =>
      }
      vols += rf -> vol
    }
    vols
  }

  lazy val rhos : Map[(VaRRiskFactor, VaRRiskFactor), Double] = {
    var map = Map.empty[(VaRRiskFactor, VaRRiskFactor), Double]
    riskFactors.zipWithIndex.foreach{
      case (rf1, i) =>
        riskFactors.zipWithIndex.foreach{
          case (rf2, j) =>
            map += (rf1, rf2) -> rhoMatrix(i, j)
        }
    }
    map
  }
  def historicCorrelation(rf1 : VaRRiskFactor, rf2 : VaRRiskFactor) : Double = rhos(rf1, rf2)

}

object RiskFactorStatistics{
  /**
   * Very rough scaling to take account of how many days have fixed
   */
  def freightFrontPeriodScaling(mkt : Market.BalticFuturesMarket, marketDay : DayAndTime) : Double = {
    val frontMonth = mkt.nthPeriod(marketDay, 0).asInstanceOf[Month]
    val numDaysLeft = mkt.lastTradingDay(frontMonth) - marketDay.day
    2.0 * numDaysLeft / 30
  }
}
