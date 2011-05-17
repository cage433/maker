package starling.curves

import starling.daterange._
import starling.marketdata._
import collection.immutable.List
import starling.utils.ImplicitConversions._
import starling.db.{MarketDataReaderAdapter, MarketDataReader}


class RecordingMarketDataReader(reader: MarketDataReader) extends MarketDataReaderAdapter(reader) {
  private val recorded0 = new scala.collection.mutable.HashMap[(ObservationPoint, MarketDataKey), MarketData]()

  //This is a bit strange/inconsistent. We only record the calls to this read method, not the more general one
  //as we know that this is the call used when populating a real environment, rather then the call to find
  //the avaliable data
  override def read(observationPoint: ObservationPoint, key: MarketDataKey) = {
    super.read(observationPoint, key).update { marketData => recorded0 += ((observationPoint, key) -> marketData) }
  }

  def recorded : Set[(ObservationPoint, MarketDataKey, MarketData)] = {
    recorded0.toList.map{ case((t,k),d) => (t,k,d) }.toSet
  }
}