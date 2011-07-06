package starling.db

import starling.daterange._
import starling.marketdata._
import starling.curves.MarketDataSlice
import starling.market.CommodityMarket


class MarketDataReaderMarketDataSlice(
    reader:MarketDataReader, observationPoint:ObservationPoint, observationTimeOverrides:Map[MarketDataType,ObservationTimeOfDay]=Map()) extends MarketDataSlice {
  def read(key: MarketDataKey) = {
    val point = observationPoint.copyTime(observationTimeOverrides.get(key.dataType))
    reader.read(TimedMarketDataKey(point, key))
  }

  def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint) : PriceFixingsHistoryData = {
    key.read(observationPoint, reader)
  }
}




































