package starling.varcalculator

import starling.quantity.{Quantity, UOM}
import starling.curves.ObservationDay
import starling.daterange.{DayAndTime, Day}


/** A price history by risk factor
 */
case class RiskFactorPriceHistory(
  marketDay : DayAndTime, 
  riskFactors : List[VaRRiskFactor],
  observationDays : List[ObservationDay],
  prices : Map[(VaRRiskFactor, ObservationDay), (Quantity, Quantity)]
 ){
  require (observationDays == observationDays.sortWith(_<_), "Snapshots must be in ascending order")

  private def fillInGaps(list : List[Option[(Quantity, Quantity)]], rf:RiskFactor) : List[(Quantity, Quantity)] = {
    def recurse(acc : List[(Quantity, Quantity)], list : List[Option[(Quantity, Quantity)]]) : List[(Quantity, Quantity)] = {
      list match {
        case Nil => acc.reverse
        case Some(q) :: rest => recurse(q :: acc, rest)
        case None :: rest => acc match {
          case q :: _ => recurse(q :: acc, rest)
          case      _ => {
            val aaa = list.find(_.isDefined) match {
              case Some(x) => x.get
              case None => throw new Exception("Can't find a price to fill in the gap for " + rf + " Maybe there are no prices ")
            }
            recurse(aaa :: acc, rest)
          }
        }
      }
    }
    recurse(Nil, list)
  }

  /** Return a new price history, filling in gaps for any missing curves with the previous known price.
   *  If there is no previous then the price soonest after is used.
   */
  def fillInMissingData : RiskFactorPriceHistory = {
    var results = Map.empty[(VaRRiskFactor, ObservationDay), (Quantity, Quantity)]
    for (
      rf <- riskFactors
    ){
      val priceList : List[Option[(Quantity, Quantity)]] = observationDays.map(prices.get(rf, _))
      val filledInPrices = fillInGaps(priceList, rf)
      observationDays.zip(filledInPrices).foreach {
        case (day, p) => results += (rf, day) -> p
      }
    }
    RiskFactorPriceHistory(marketDay, riskFactors, observationDays, results)
  }

  /** Returns all known prices for the latest snapshot
   */
  lazy val latestPrices : Map[VaRRiskFactor, Quantity] = {
    val latestObservationDay = observationDays.last
    val keys : Iterable[(VaRRiskFactor, ObservationDay)]= prices.keySet.filter(_._2 == latestObservationDay)
    Map.empty ++ keys.map{case (rf, day) => rf -> prices((rf, day))._1}
  }
  lazy val riskFactorUOMS : Map[VaRRiskFactor, UOM] = Map.empty ++ latestPrices.map{case (rf, qty) => rf -> qty.uom}

}