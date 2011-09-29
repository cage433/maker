package starling.curves

import starling.daterange._
import starling.marketdata._
import collection.immutable.List
import starling.utils.ImplicitConversions._
import scala.collection.JavaConversions._
import starling.db.{MarketDataReaderAdapter, MarketDataReader}
import com.google.common.collect.MapMaker


class RecordingMarketDataReader(reader: MarketDataReader) extends MarketDataReaderAdapter(reader) {
  private val recorded0 = new MapMaker().concurrencyLevel(16).makeMap[(TimedMarketDataKey), MarketData]

  //This is a bit strange/inconsistent. We only record the calls to this read method, not the more general one
  //as we know that this is the call used when populating a real environment, rather then the call to find
  //the avaliable data
  override def read(timedKey: TimedMarketDataKey) = {
    super.read(timedKey).update { marketData => recorded0.put(timedKey, marketData) }
  }

  def recorded : Set[(TimedMarketDataKey, MarketData)] = recorded0.toSet
}