package starling.db

import starling.marketdata._
import starling.daterange._
import starling.utils.cache.CacheFactory
import scalaz.Scalaz._
import collection.immutable._


class CachingMdDB(adapted: MdDB) extends AdaptingMdDB(adapted) {
  val cache = CacheFactory.getCache("mddb-cache", unique=true)

  override def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName,
    observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
    marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketDataRows)] = {

    def filterInMemory(result: List[(TimedMarketDataKey, MarketDataRows)]): List[(TimedMarketDataKey, MarketDataRows)] = {
      val truePredicate = (e: TimedMarketDataKey) => true

      val observationDayPredicate =
        observationDays.fold(days => (e: TimedMarketDataKey) => days.contains(e.observationPoint.day), truePredicate)

      val observationTimePredicate =
        observationTimes.fold(times => (e: TimedMarketDataKey) => times.contains(e.observationPoint.timeOfDay), truePredicate)

      val marketDataKeysPredicate =
        marketDataKeys.fold(keys => (e: TimedMarketDataKey) => keys.contains(e.key), truePredicate)

      result.filter(element => observationDayPredicate(element._1)
        && observationTimePredicate(element._1)
        && marketDataKeysPredicate(element._1))
    }

    val dataKey = (version, mds.toSet, marketDataType, observationDays, observationTimes, marketDataKeys)

    cache.memoizeZ(dataKey) { _ =>
      val allDataKey = (version, mds.toSet, marketDataType)

      (observationDays, observationTimes, marketDataKeys) match {
        case (None, None, None) => cache.memoizeZ(allDataKey) { _ => super.query(version, mds, marketDataType, None, None, None) }
        case _ => {
          cache.get[allDataKey.type, List[(TimedMarketDataKey, MarketDataRows)]](allDataKey).fold(allData => filterInMemory(allData),
            super.query(version, mds, marketDataType, observationDays, observationTimes, marketDataKeys))
        }
      }
    }
  }
}