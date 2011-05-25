package starling.marketdata

import starling.market._
import collection.immutable.TreeMap
import starling.daterange.StoredFixingPeriod
import starling.utils.conversions.Tuple2Ordering
import starling.quantity.Quantity
import starling.pivot.MarketValue
import collection.{Map, SortedMap}


case class PriceFixingsHistoryData(fixings: SortedMap[(Level, StoredFixingPeriod), MarketValue]) extends MarketData {
  def fixingFor(level: Level, period: StoredFixingPeriod): MarketValue = {
    fixings.getOrElse( ( level,period), throw new Exception("No fixing found for " + level + " " + period +
      ", only have levels: " + fixings.keys.map(_._1) + ", months: " + fixings.keys.map(_._2)))
  }

  def +(other: PriceFixingsHistoryData) = {
    val combinedFixings = fixings ++ other.fixings; PriceFixingsHistoryData.create(combinedFixings)
  }
}

object PriceFixingsHistoryData {
  def sum(data: Traversable[PriceFixingsHistoryData]): PriceFixingsHistoryData = {
    data.foldLeft(PriceFixingsHistoryData(emptyFixings))(_ + _)
  }

  def create(prices: Map[(Level, StoredFixingPeriod), MarketValue]): PriceFixingsHistoryData = {
    PriceFixingsHistoryData(emptyFixings ++ prices)
  }

  def create(level:Level, period:StoredFixingPeriod, value:Quantity):PriceFixingsHistoryData = create(Map( (level,period) → MarketValue.quantity(value)))

  def create(prices: List[((Level, StoredFixingPeriod), MarketValue)]): PriceFixingsHistoryData = {
    PriceFixingsHistoryData(emptyFixings ++ prices)
  }

  private val emptyFixings = TreeMap.empty[(Level, StoredFixingPeriod), MarketValue](new Tuple2Ordering[Level, StoredFixingPeriod])
}