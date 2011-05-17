package starling.db

import starling.daterange.{Day, ObservationTimeOfDay, ObservationPoint}
import starling.marketdata.{MarketDataType, PriceValidator, MarketDataKey, PriceData}

class MarketDataReaderAdapter(reader: MarketDataReader) extends MarketDataReader {

  def identifier = reader.identifier

  def marketDataTypes = reader.marketDataTypes

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataType) =
    reader.readAllObservationDayAndMarketDataKeys(marketDataType)

  def read(marketDataType: MarketDataType, observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
           keys: Option[Set[MarketDataKey]]) = reader.read(marketDataType, observationDays, observationTimes, keys)

}



