package starling.db

import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import java.lang.String
import collection.immutable.Map


trait MarketDataSource { self =>
  def read(day: Day): Map[(Day, Day, MarketDataType), List[MarketDataEntry]]

  def asserting(): MarketDataSource = new MarketDataSource {
    def read(day: Day) = {
      val map: Map[(Day, Day, MarketDataType), List[MarketDataEntry]] = self.read(day)
      val result = map.updateIt(r => duplicateTimedKeys(r).require(_.isEmpty, "source: %s produced duplicate 'timed' keys: " % self))
      result
    }

    def duplicateTimedKeys(map: Map[(Day, Day, MarketDataType), List[MarketDataEntry]]): List[TimedMarketDataKey] =
      map.values.flatMap(duplicateTimedKeys).toList
  }

  def +(other: MarketDataSource): MarketDataSource = new CompositeMarketDataSource(this, other)

  protected def containsDistinctTimedKeys(entries: List[MarketDataEntry]): Boolean = duplicateTimedKeys(entries).isEmpty

  protected def duplicateTimedKeys(entries: List[MarketDataEntry]) = entries.map(_.timedKey).duplicates

  def description: List[String] = Nil
}

class AdaptingMarketDataSource(adaptee: MarketDataSource) extends MarketDataSource {
  def read(day: Day) = adaptee.read(day)
}

case class CompositeMarketDataSource(sources: MarketDataSource*) extends MarketDataSource {
  def read(day: Day) = sources.map(_.read(day)).fold(Map.empty[(Day, Day, MarketDataType), List[MarketDataEntry]])(_ ++ _)
}