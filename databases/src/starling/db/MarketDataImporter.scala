package starling.db

import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import starling.utils.Log


class MarketDataImporter(marketDataStore: MarketDataStore) {
  def getUpdates(observationDay: Day, marketDataSets: MarketDataSet*): Map[MarketDataSet, Iterable[MarketDataUpdate]] = {
    marketDataSets.map(marketDataSet => {
      val marketDataForSet = (marketDataStore.marketData _).applyLast(marketDataSet)

      marketDataStore.sourceFor(marketDataSet).map { marketDataSource =>
        Log.infoWithTime("importing market data " + marketDataSet + " for " + observationDay) {
          val externalData = marketDataSource.asserting.read(observationDay).mapValues(_.filterNot(_.isEmpty))
          Log.debug("Amount of external data: " + externalData.mapValues(_.size))

          val keysWhichShouldBePresent = externalData.values.flatMap(_.map(_.timedKey)).toList
          val presentData: Map[(ObservationPoint, MarketDataKey), VersionedMarketData] =
            externalData.keys.flatMap(marketDataForSet).toList.map { case (point, key, data) => (point,key) → data }.toMap

          val presentKeys = presentData.keySet
          val extraKeys = presentKeys \\ keysWhichShouldBePresent
          val deletes = extraKeys.map{ case(observationPoint, key) => MarketDataUpdate(observationPoint, key, None, presentData.get((observationPoint, key))) }.toList
          val filtered = externalData.values.flatten.filter { entry => presentData.get(entry.timedKey) != Some(entry.data) }
          val saves = filtered.toList.flatMapO(entry => entry.toSave(presentData.get(entry.timedKey))).toList
          marketDataSet → (saves ::: deletes)
        }
      }.getOrElse(marketDataSet → Nil)
    } ).toMap
  }
}