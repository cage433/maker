package starling.db

import starling.daterange._
import starling.marketdata._
import starling.curves.MarketDataSlice
import starling.market.CommodityMarket


class MarketDataReaderMarketDataSlice(reader:MarketDataReader, observationPoint:ObservationPoint) extends MarketDataSlice {
  def read(key: MarketDataKey) = reader.read(TimedMarketDataKey(observationPoint, key))

  def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint) : PriceFixingsHistoryData = {
    key.read(observationPoint, reader)
  }
}





































