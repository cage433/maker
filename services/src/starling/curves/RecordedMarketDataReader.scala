package starling.curves

import starling.db.MarketDataReader
import starling.daterange.{ObservationTimeOfDay, Day}
import starling.marketdata.{TimedMarketDataKey, MarketData, MarketDataType, MarketDataKey}

import starling.utils.ImplicitConversions._

/**
 * A MarketDataReader which only holds data which was recorded
 */

class RecordedMarketDataReader(val identifier:String, recorded:List[(TimedMarketDataKey, MarketData)]) extends MarketDataReader {
  def marketDataTypes = recorded.map(_.head.dataType).toSet.toList

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataType): List[TimedMarketDataKey] = {
    recorded.filter(_.head.dataType == marketDataType).map(_.head)
  }

  def read(
    marketDataType: MarketDataType,
    observationDays: Option[Set[Option[Day]]],
    observationTimes: Option[Set[ObservationTimeOfDay]],
    keys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {

    recorded.filter(_.head.dataType == marketDataType).
    filter { case (timedKey,data) => observationDays.map( days => days.contains(timedKey.day)).getOrElse(true)}.
    filter { case (timedKey,data) => observationTimes.map( times => times.contains(timedKey.timeOfDay)).getOrElse(true)}.
    filter { case (timedKey,data) => keys.map( ks => ks.contains(timedKey.key)).getOrElse(true) }
  }

  override def toString = identifier + "\n  " + recorded.mkString("\n  ")
}