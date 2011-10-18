package starling.db

import starling.daterange.{Day, ObservationTimeOfDay, ObservationPoint}
import starling.marketdata._

class MarketDataReaderAdapter(reader: MarketDataReader) extends MarketDataReader {

  def identifier = reader.identifier

  def marketDataTypes = reader.marketDataTypes

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataTypeName) =
    reader.readAllObservationDayAndMarketDataKeys(marketDataType)

  def read(marketDataType: MarketDataTypeName, observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
           keys: Option[Set[MarketDataKey]]) = reader.read(marketDataType, observationDays, observationTimes, keys)

}



