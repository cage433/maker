package starling.db

import starling.richdb.RichDB
import starling.utils.StarlingXStream
import starling.marketdata.{MarketData, MarketDataType, MarketDataKey}
import starling.utils.sql.QueryBuilder
import starling.curves.{MissingMarketDataException, MarketDataSlice}
import starling.gui.api.PricingGroup
import starling.daterange.{ObservationPoint}

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