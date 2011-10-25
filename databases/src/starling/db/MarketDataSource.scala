package starling.db

import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import java.lang.String
import starling.scheduler.TaskDescription
import starling.databases.MarketDataEventSource

trait MarketDataSource { self =>
  val name: String = getClass.getSimpleName

  def read(day: Day): MultiMap[(Day, Day, MarketDataTypeName), MarketDataEntry]

  def asserting(): MarketDataSource = new MarketDataSource {
    def read(day: Day) = {
      val map: MultiMap[(Day, Day, MarketDataTypeName), MarketDataEntry] = self.read(day)
      val result = map.updateIt(r => duplicateTimedKeys(r).require(_.isEmpty, "source: %s produced duplicate 'timed' keys: " % self))
      result
    }

    def duplicateTimedKeys(map: MultiMap[(Day, Day, MarketDataTypeName), MarketDataEntry]): List[TimedMarketDataKey] =
      map.values.flatMap(duplicateTimedKeys).toList
  }

  protected def containsDistinctTimedKeys(entries: List[MarketDataEntry]): Boolean = duplicateTimedKeys(entries).isEmpty

  protected def duplicateTimedKeys(entries: List[MarketDataEntry]) = entries.map(_.timedKey).duplicates

  def description: List[String] = Nil

  def eventSources(marketDataStore: MarketDataStore): List[MarketDataEventSource] = Nil
  def availabilityTasks(marketDataStore: MarketDataStore): List[TaskDescription] = Nil
}

class AdaptingMarketDataSource(adaptee: MarketDataSource) extends MarketDataSource {
  override val name = adaptee.name
  def read(day: Day) = adaptee.read(day)
}