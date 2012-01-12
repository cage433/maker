package starling.db

import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import java.lang.String
import collection.immutable.Map


trait MdDB {
  def checkIntegrity(): Unit
  def readAll(): Unit
  def marketDataSetNames(): List[String]
  def observationDaysByMarketDataSet: MultiMap[String, Day]
  def latestVersionForMarketDataSets(): Map[MarketDataSet, Int]
  def latestObservationDaysFor(marketDataSets: List[MarketDataSet], marketDataType: MarketDataTypeName): Option[Day]
  def latestExcelVersions(): Map[MarketDataSet, Int]
  def store(data: List[MarketDataUpdate], marketDataSet: MarketDataSet): SaveResult
  def maxVersionForMarketDataSetNames(names: List[String]): Option[Int]

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataTypeName, marketDataSet: MarketDataSet): Map[TimedMarketDataKey, VersionedMarketData]

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName): Set[TimedMarketDataKey]

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketDataRows)]

  def readLatest(id: MarketDataID): Option[VersionedMarketData]
  def latestVersionForAllMarketDataSets(): Map[MarketDataSet, Int] = latestVersionForMarketDataSets() ++ latestExcelVersions()
}