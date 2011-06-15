package starling.db

import starling.daterange._
import starling.utils.Log

import starling.utils.ImplicitConversions._


class MarketDataImporter(marketDataStore: MarketDataStore) {
  def getUpdates(observationDay: Day, marketDataSets: MarketDataSet*) = marketDataSets.toMapWithValues(marketDataSet => {
    val getMarketData = (marketDataStore.marketData _).applyLast(marketDataSet)

    marketDataStore.sourceFor(marketDataSet).flatMapL { marketDataSource => Log.infoWithTime("importing: " + marketDataSet.name) {
      val externalData = marketDataSource.asserting.read(observationDay).mapValues(_.filterNot(_.isEmpty))
      val existingData = externalData.keys.flatMap(getMarketData).toMap

      val saves = externalData.values.flatten.collect {
        case entry if existingData.get(entry.timedKey) != Some(entry.data) => entry.toSave(existingData.get(entry.timedKey))
      }.toList.somes

      val deletes = {
        val existingSaves = existingData.collectValues { case VersionedMarketData.Save(savedData) => savedData }
        val keysWhichShouldBePresent = externalData.values.flatMap(_.map(_.timedKey)).toList
        val extraKeys = existingSaves.keySet \\ keysWhichShouldBePresent

        extraKeys.map(timedKey => MarketDataUpdate(timedKey, None, existingSaves.get(timedKey))).toList
      }

      saves ::: deletes
    } }
  } )
}