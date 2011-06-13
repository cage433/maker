package starling.db

import starling.daterange._
import starling.utils.Log

import starling.utils.ImplicitConversions._


class MarketDataImporter(marketDataStore: MarketDataStore) {
  def getUpdates(observationDay: Day, marketDataSets: MarketDataSet*) = marketDataSets.toMapWithValues(marketDataSet => {
    val getMarketData = (marketDataStore.marketData _).applyLast(marketDataSet)

    marketDataStore.sourceFor(marketDataSet).flatMapL { marketDataSource =>
      Log.infoWithTime("importing market data " + marketDataSet + " for " + observationDay) {
        val externalData = marketDataSource.asserting.read(observationDay).mapValues(_.filterNot(_.isEmpty))
        Log.debug("Amount of external data: " + externalData.mapValues(_.size))

        val existingSaves = externalData.keys.flatMap(getMarketData).toMap
          .collectValues { case VersionedMarketData.Save(savedData) => savedData }

        val saves = {
          val filtered = externalData.values.flatten.filter(entry => existingSaves.get(entry.timedKey) != Some(entry.data))

          filtered.toList.flatMapO(entry => entry.toSave(existingSaves.get(entry.timedKey)))
        }

        val deletes = {
          val keysWhichShouldBePresent = externalData.values.flatMap(_.map(_.timedKey)).toList
          val extraKeys = existingSaves.keySet \\ keysWhichShouldBePresent

          extraKeys.map(timedKey => MarketDataUpdate(timedKey, None, existingSaves.get(timedKey))).toList
        }

        saves ::: deletes
      }
    }
  } )
}