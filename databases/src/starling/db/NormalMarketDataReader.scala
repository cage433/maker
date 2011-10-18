package starling.db

import starling.marketdata._
import starling.gui.api._
import starling.daterange._


class NormalMarketDataReader(marketDataStore: MarketDataStore, marketDataIdentifier: MarketDataIdentifier)
  extends MarketDataReader {

  def identifier = marketDataIdentifier.toString

  def marketDataTypes = marketDataStore.marketDataTypes(marketDataIdentifier)

  def read(marketDataType: MarketDataTypeName, observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
           keys: Option[Set[MarketDataKey]]) = {

    marketDataStore.query(marketDataIdentifier, marketDataType, observationDays, observationTimes, keys)
  }

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataTypeName) = {
    marketDataStore.queryForObservationDayAndMarketDataKeys(marketDataIdentifier, marketDataType)
  }
}

object NormalMarketDataReader {
  def apply(marketDataStore: MarketDataStore, selection: MarketDataSelection) =
    new NormalMarketDataReader(marketDataStore, marketDataStore.latestMarketDataIdentifier(selection))
}