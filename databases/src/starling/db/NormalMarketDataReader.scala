package starling.db

import starling.marketdata._
import starling.gui.api._
import starling.daterange._
import starling.market.{Market, FuturesMarket, CommodityMarket}
import starling.curves.ObservationDay

class NormalMarketDataReader(marketDataStore: MarketDataStore, marketDataIdentifier: MarketDataIdentifier)
  extends MarketDataReader {

  def identifier = marketDataIdentifier.toString

  def marketDataTypes = marketDataStore.marketDataTypes(marketDataIdentifier)

  def read(marketDataType: MarketDataType, observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
           keys: Option[Set[MarketDataKey]]) = {

    marketDataStore.query(marketDataIdentifier, marketDataType, observationDays, observationTimes, keys)
  }

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataType) = {
    marketDataStore.queryForObservationDayAndMarketDataKeys(marketDataIdentifier, marketDataType)
  }
}
