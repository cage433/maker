package starling.db

import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import java.lang.String
import starling.utils._
import collection.immutable.{Iterable, Map}
import scalaz.Scalaz._
import starling.richdb.RichResultSetRow

object MdDB {
  def apply(db: DBTrait[RichResultSetRow]): MdDB = if (false) {
    val fast = VersionTransformingMdDB(new NewSchemaMdDB(db, new MarketDataTypes(ReferenceDataLookup.Null)), db).toIdentity
    val slow = VersionTransformingMdDB(new SlowMdDB(db), db).reverse

    VerifyingDynamicProxy.create(fast, slow, throwFailures = false)
  } else {

    new SlowMdDB(db)
//    new NewSchemaMdDB(db)
  }
}

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
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)]

  def readLatest(id: MarketDataID): Option[VersionedMarketData]
  def latestVersionForAllMarketDataSets(): Map[MarketDataSet, Int] = latestVersionForMarketDataSets() ++ latestExcelVersions()
}