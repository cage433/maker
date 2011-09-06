package starling.db

import starling.richdb.RichDB
import starling.instrument.utils.StarlingXStream
import starling.dbx.QueryBuilder
import starling.curves.{MissingMarketDataException, MarketDataSlice}
import starling.gui.api.PricingGroup
import starling.daterange.{ObservationPoint}
import starling.marketdata.{TimedMarketDataKey, MarketData, MarketDataType, MarketDataKey}

/**
 * Uniquely identifies a market data object in the database. Versioning is added
 * on top of this by the VersionedDatabase mixin.
 */
case class MarketDataID(
        observationPoint: ObservationPoint,
        marketDataSet : MarketDataSet,
        subTypeKey : MarketDataKey
        ) {
  def conditions : Map[String, Any] = Map(
    "observationTime" -> observationPoint.timeName,
    "observationDay" -> observationPoint.day.getOrElse(null),
    "marketDataSet" -> marketDataSet.name,
    "marketDataType" -> StarlingXStream.write(subTypeKey.dataType),
    "marketDataKey" -> StarlingXStream.write(subTypeKey))
}

object MarketDataID {
  def apply(timedKey: TimedMarketDataKey, marketDataSet: MarketDataSet) =
    new MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key)
}