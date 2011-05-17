package starling.db

import starling.daterange._
import starling.marketdata._
import starling.curves.MarketDataSlice
import starling.market.CommodityMarket


class MarketDataReaderMarketDataSlice(reader:MarketDataReader, observationPoint:ObservationPoint) extends MarketDataSlice {
  def read(key: MarketDataKey) = {
    reader.read(observationPoint, key)
  }

  def fixings(market:CommodityMarket, observationPoint: ObservationPoint) = {
    PriceFixingsHistoryDataKey(market).read(observationPoint, reader)
  }
}





































