package starling.curves

import starling.db.MarketDataReader
import starling.daterange.{ObservationTimeOfDay, Day}
import starling.utils.ImplicitConversions._
import starling.marketdata._
import scalaz.Scalaz._

/**
 * A MarketDataReader which only holds data which was recorded
 */

class RecordedMarketDataReader(val identifier:String, recorded:List[(TimedMarketDataKey, MarketData)], val marketDataTypes: MarketDataTypes)
  extends MarketDataReader {

  def availableMarketDataTypes = recorded.map(_.head.typeName |> (marketDataTypes.fromName _)).toSet.toList

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataTypeName): List[TimedMarketDataKey] = {
    recorded.filter(_.head.typeName == marketDataType).map(_.head)
  }

  def read(
    marketDataType: MarketDataTypeName,
    observationDays: Option[Set[Option[Day]]],
    observationTimes: Option[Set[ObservationTimeOfDay]],
    keys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketDataRows)] = {

    recorded.filter(_.head.typeName == marketDataType).
    filter { case (timedKey,data) => observationDays.map( days => days.contains(timedKey.day)).getOrElse(true)}.
    filter { case (timedKey,data) => observationTimes.map( times => times.contains(timedKey.timeOfDay)).getOrElse(true)}.
    filter { case (timedKey,data) => keys.map( ks => ks.contains(timedKey.key)).getOrElse(true) }.
    map { case (key,marketData) => (key, MarketDataRows(marketDataTypes.fromName(key.typeName).castRows(key.key, marketData).map(_.value).toList)) }

  }

  override def toString = identifier + "\n  " + recorded.mkString("\n  ")
}