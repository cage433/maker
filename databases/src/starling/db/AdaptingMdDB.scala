package starling.db

import starling.marketdata._
import starling.daterange._
import java.lang.String
import collection.immutable._


class AdaptingMdDB(adapted: MdDB) extends MdDB {
  def checkIntegrity() = adapted.checkIntegrity
  def readAll() = adapted.readAll
  def marketDataSetNames() = adapted.marketDataSetNames
  def observationDaysByMarketDataSet = adapted.observationDaysByMarketDataSet
  def latestVersionForMarketDataSets() = adapted.latestVersionForMarketDataSets
  def latestObservationDaysFor(marketDataSets: List[MarketDataSet], marketDataType: MarketDataTypeName) =
    adapted.latestObservationDaysFor(marketDataSets, marketDataType)
  def latestExcelVersions() = adapted.latestExcelVersions
  def store(data: List[MarketDataUpdate], marketDataSet: MarketDataSet) = adapted.store(data, marketDataSet)
  def maxVersionForMarketDataSetNames(names: List[String]) = adapted.maxVersionForMarketDataSetNames(names)
  def marketDataTypes(version: Int, mds: List[MarketDataSet]) = adapted.marketDataTypes(version, mds)

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataTypeName, marketDataSet: MarketDataSet) =
    adapted.latestMarketData(from, to, marketDataType, marketDataSet)

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName) =
    adapted.queryForObservationDayAndMarketDataKeys(version, mds, marketDataType)

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName, observationDays: Option[Set[Option[Day]]],
            observationTimes: Option[Set[ObservationTimeOfDay]], marketDataKeys: Option[Set[MarketDataKey]]) = {

    adapted.query(version, mds, marketDataType, observationDays, observationTimes, marketDataKeys)
  }

  def readLatest(id: MarketDataID) = adapted.readLatest(id)
}