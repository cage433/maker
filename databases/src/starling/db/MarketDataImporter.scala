package starling.db

import starling.daterange._
import starling.utils.Log

import starling.utils.ImplicitConversions._


class MarketDataImporter(marketDataStore: MarketDataStore) extends Log {
  def getUpdates(observationDay: Day, marketDataSets: MarketDataSet*): MultiMap[MarketDataSet, MarketDataUpdate] =
    marketDataSets.toMapWithValues(marketDataSet =>
      marketDataStore.sourcesFor(marketDataSet).flatMap { marketDataSource =>
        log.infoWithTime("importing: %s (%s)" % (marketDataSet.name, marketDataSource.name)) {
          updatesFor(observationDay, marketDataSet, marketDataSource)
        }
      }
    )

  private def updatesFor(observationDay: Day, marketDataSet: MarketDataSet, marketDataSource: MarketDataSource) = {
    def logCount(name: String)(updates: List[MarketDataUpdate]) = updates.groupBy(_.tag).foreach { case (tag, updatesForTag) => {
      log.info("%s %ss for %s %s" % (updatesForTag.size, name, marketDataSet.name, tag.getOrElse("Unknown")))
      log.debug(updatesForTag.map(_.timedKey).mkString(name + ": ", "\n" + name + ": ", "\n"))
    } }

    val externalData = marketDataSource.asserting.read(observationDay).mapValues(_.filterNot(_.isEmpty))
    val existingData = externalData.keys.flatMap(key => (marketDataStore.marketData _).tupled(key.add(marketDataSet))).toMap

    val saves = externalData.values.flatten.collect {
     case entry if existingData.get(entry.timedKey).flatMap(_.data) != Some(entry.data) => entry.toSave(existingData.get(entry.timedKey))
    }.toList.somes.update(logCount("save"))

    val deletes: List[MarketDataUpdate] = {
      val existingSaves = existingData.collectValues { case VersionedMarketData.Save(savedData) => savedData }
      val keysWhichShouldBePresent = externalData.values.flatMap(_.map(_.timedKey)).toList
      val extraKeys = existingSaves.keySet -- keysWhichShouldBePresent.toSet

      extraKeys.map(timedKey => MarketDataUpdate(timedKey, None, existingSaves.get(timedKey))).toList
    }.update(logCount("delete"))

    saves ::: deletes
  }
}