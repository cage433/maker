package starling.db

import starling.daterange._
import starling.utils.Log

import starling.utils.ImplicitConversions._
import starling.marketdata.MarketData


class MarketDataImporter(marketDataStore: MarketDataStore) extends Log {
  def getUpdates(observationDay: Day, marketDataSets: MarketDataSet*) = marketDataSets.toMapWithValues(marketDataSet => {
    val getMarketData = (marketDataStore.marketData _).applyLast(marketDataSet)

    def logCount(name: String)(updates: List[MarketDataUpdate]) = updates.groupBy(_.tag).foreach {
      case (tag, updatesForTag) => log.info("%s %s for %s %s" % (updatesForTag.size, name, marketDataSet.name, tag.getOrElse("Unknown")))
    }

    marketDataStore.sourceFor(marketDataSet).flatMapL { marketDataSource => log.infoWithTime("importing: " + marketDataSet.name) {
      val externalData = marketDataSource.asserting.read(observationDay).mapValues(_.filterNot(_.isEmpty))
      val existingData = externalData.keys.flatMap(getMarketData).toMap

      val saves = externalData.values.flatten.collect {
        case entry if existingData.get(entry.timedKey).flatMap(_.data) != Some(entry.data) => entry.toSave(existingData.get(entry.timedKey))
      }.toList.somes.update(logCount("saves"))

      val deletes = {
        val existingSaves = existingData.collectValues { case VersionedMarketData.Save(savedData) => savedData }
        val keysWhichShouldBePresent = externalData.values.flatMap(_.map(_.timedKey)).toList
        val extraKeys = existingSaves.keySet -- keysWhichShouldBePresent.toSet

        extraKeys.map(timedKey => MarketDataUpdate(timedKey, None, existingSaves.get(timedKey))).toList
      }.update(logCount("deletes"))

      saves ::: deletes
    } }
  } )
}