package starling.db

import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import java.lang.String
import collection.immutable._
import starling.richdb.RichResultSetRow


object VersionTransformingMdDB {
  def apply(adapted: MdDB, db: DBTrait[RichResultSetRow]): VersionTransformingMdDB = {
    val versionToCommit = db.queryWithResult("SELECT id, version FROM MarketDataCommit")
      { rs => (rs.getInt("version"), rs.getInt("id")) }.toMap

    VersionTransformingMdDB(adapted, db, versionToCommit.withDefault(-_), versionToCommit.reverse.withDefault(-_))
  }
}

case class VersionTransformingMdDB(adapted: MdDB, db: DBTrait[RichResultSetRow], from: Map[Int, Int], to: Map[Int, Int])
  extends AdaptingMdDB(adapted) {

  def reverse = copy(from = to, to = from)

  // removes timing differences between slow & fast
  def toIdentity = copy(from = from.map(p => (p._1, p._1)), to = to.map(p => (p._1, p._1)))

  override def latestVersionForMarketDataSets() = super.latestVersionForMarketDataSets.mapValuesEagerly(out)
  override def latestExcelVersions() = super.latestExcelVersions.mapValuesEagerly(out)
  override def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet) = out(super.store(data.map(in), marketDataSet))
  override def maxVersionForMarketDataSetNames(names: List[String]) = super.maxVersionForMarketDataSetNames(names).map(out)
  override def marketDataTypes(version: Int, mds: List[MarketDataSet]) = super.marketDataTypes(in(version), mds)

  override def latestMarketData(from: Day, to: Day, marketDataType: MarketDataTypeName, marketDataSet: MarketDataSet) =
    super.latestMarketData(from, to, marketDataType, marketDataSet).mapValuesEagerly(out)

  override def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName) =
    super.queryForObservationDayAndMarketDataKeys(in(version), mds, marketDataType)

  override def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName,
                     observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
                     marketDataKeys: Option[Set[MarketDataKey]]) = {

    super.query(in(version), mds, marketDataType, observationDays, observationTimes, marketDataKeys)
  }

  override def readLatest(id: MarketDataID) = super.readLatest(id).map(out)

  private def in(version: Int): Int = from(version).info(commitId => "versionToCommit(%d) = %d" % (version, commitId))
  private def in(marketData: VersionedMarketData): VersionedMarketData = marketData.copy(version = in(marketData.version))
  private def in(update: MarketDataUpdate): MarketDataUpdate = update.copy(existingData = update.existingData.map(in))

  private def out(commitId: Int): Int = to(commitId).info(version => "commitToVersion(%d) = %d" % (commitId, version))
  private def out(marketData: VersionedMarketData): VersionedMarketData = marketData.copy(version = out(marketData.version))
  private def out(saveResult: SaveResult): SaveResult = saveResult.copy(maxVersion = out(saveResult.maxVersion))
}