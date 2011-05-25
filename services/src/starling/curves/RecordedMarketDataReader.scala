package starling.curves

import starling.db.MarketDataReader
import starling.daterange.{ObservationTimeOfDay, Day, ObservationPoint}
import starling.marketdata.{MarketData, MarketDataType, MarketDataKey}

/**
 * A MarketDataReader which only holds data which was recorded
 */

class RecordedMarketDataReader(val identifier:String, recorded:List[(ObservationPoint,MarketDataKey,MarketData)]) extends MarketDataReader {

  def marketDataTypes = recorded.map(_._2.dataType).toSet.toList

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataType):List[(ObservationPoint, MarketDataKey)] = {
    recorded.
      filter { case (point,key,data) => key.dataType == marketDataType}.
      map { case (point,key,data) => (point,key) }
  }

  def read(
    marketDataType: MarketDataType,
    observationDays: Option[Set[Option[Day]]],
    observationTimes: Option[Set[ObservationTimeOfDay]],
    keys: Option[Set[MarketDataKey]]): List[(ObservationPoint, MarketDataKey, MarketData)] = {

    recorded.filter { case (point,key,data) => key.dataType == marketDataType}.
    filter { case (point,key,data) => observationDays.map( days => days.contains(point.day)).getOrElse(true)}.
    filter { case (point,key,data) => observationTimes.map( times => times.contains(point.timeOfDay)).getOrElse(true)}.
    filter { case (point,key,data) => keys.map( ks => ks.contains(key)).getOrElse(true) }
  }
}