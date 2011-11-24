package starling.db

import starling.marketdata._
import starling.richdb.RichResultSetRow
import starling.daterange._
import starling.instrument.utils.StarlingXStream
import starling.utils.Pattern._
import java.lang.String
import starling.utils._
import scalaz._
import Scalaz._
import collection.immutable.{Iterable, Map}
import sql.PersistAsBlob
import starling.dbx._
import starling.gui.api._
import QueryBuilder._
import starling.utils.ImplicitConversions._
import starling.pivot.{Field => PField}

/**
 * Thrown when there is no market data available for a particular day
 */
case class NoMarketDataForDayException(observationDay: Day, m: String) extends Exception(m)

case class MarketDataSet(name: String, priority: Int) {
  def isExcel = name.startsWith(MarketDataSet.excelPrefix)
  def stripExcel = name.stripPrefix(MarketDataSet.excelPrefix)
}

object MarketDataSet extends StarlingEnum(classOf[MarketDataSet], (m: MarketDataSet) => m.name) {
  val excelPrefix = "Excel:"

  def excel(name: String) = if (name == "Official:Metals") MarketDataSet.ManualMetals else {
    MarketDataSet(name.prefixWith(excelPrefix), Integer.MAX_VALUE)
  }

  def fromExcel(marketDataSet: MarketDataSet) = if (!marketDataSet.name.startsWith(excelPrefix)) None else {
    Some(marketDataSet.name.stripPrefix(excelPrefix))
  }

  override def fromName(name: String): MarketDataSet = if (name.startsWith(excelPrefix)) MarketDataSet(name, Integer.MAX_VALUE) else {
    super.fromName(name)
  }

  val LIM = MarketDataSet("LIM", 100) //Oil and Metals VAR
  val System = MarketDataSet("System", 200)
  val Crude = MarketDataSet("Crude", 300)
  val LondonDerivatives = MarketDataSet("London Derivatives", 400)
  val GasolineRoW = MarketDataSet("Gasoline RoW", 500)
  val GasOil = MarketDataSet("Gas Oil", 501)
  val Naphtha = MarketDataSet("Naphtha", 502)
  val BarryEckstein = MarketDataSet("Barry Eckstein", 600)
  val LondonDerivativesOptions = MarketDataSet("London Derivatives Options", 700)
  val LimMetals = MarketDataSet("LimMetals", 800) //Refined Metals
  // val Neptune = MarketDataSet("Neptune", 1000)
  val ManualMetals = MarketDataSet("ManualMetals", 1100)
  val Starling = MarketDataSet("Starling", 1200)
}

object MarketDataStore {

  import MarketDataSet._

  val pricingGroupsDefinitions = MultiMap[PricingGroup, MarketDataSet](
    PricingGroup.Metals ->> (ManualMetals, LimMetals),
    PricingGroup.LimOnly ->> LIM,
    PricingGroup.System ->> (Starling, LIM, System),
    PricingGroup.Crude ->> (Starling, LIM, Crude),
    PricingGroup.LondonDerivatives ->> (Starling, LondonDerivatives, LIM),
    PricingGroup.GasolineRoW ->> (Starling, GasolineRoW, LIM),
    PricingGroup.GasOil ->> (Starling, GasOil, LIM),
    PricingGroup.Naphtha ->> (Starling, Naphtha, LIM),
    PricingGroup.BarryEckstein ->> (Starling, BarryEckstein, System, LIM),
    PricingGroup.LondonDerivativesOptions ->> (Starling, LondonDerivativesOptions, System, LIM)
  )

  val manuallyEditableMarketDataSets = Set(ManualMetals, Starling)
  val pricingGroups = pricingGroupsDefinitions.keySet.toList
  def pricingGroupForName(name: String): PricingGroup = pricingGroups.find(_.name == name).get

  def editableMarketDataSetFor(pricingGroup: PricingGroup) = pricingGroupsDefinitions.get(pricingGroup).flatMap {
    sets => (manuallyEditableMarketDataSets & sets.toSet).headOption
  }

  def applyOverrideRule(marketDataType: MarketDataType, allDataForKeyAndDay: List[Map[PField, Any]]): List[Map[PField, Any]] = {
    val dataAsMaps: Map[Map[PField, Any], Map[PField, Any]] = Map() ++ allDataForKeyAndDay.map(marketDataType.splitByFieldType(_))
    val m = scala.collection.mutable.HashMap[Map[PField, Any], Map[PField, Any]]()

    dataAsMaps.foreach { case (k, v) => {
      m(k) = v
    } }

    m.map { case (k, v) => k ++ v }.toList
  }
}

/**
 * Wraps a data type to provide version information.
 */

trait MarketDataStore {
  def marketDataSets: Set[MarketDataSet]

  val pricingGroupsDefinitions = MarketDataStore.pricingGroupsDefinitions
  val pricingGroups = MarketDataStore.pricingGroups

  def readAll()

  def excelDataSets: List[String]

  def importData(marketDataSelection: MarketDataSelection, observationDay: Day): SaveResult
  def importFor(observationDay: Day, marketDataSets: MarketDataSet*): SaveResult

  def latest(selection: MarketDataSelection): Int

  def latestExcelVersions: Map[MarketDataSet, Int]

  def latestMarketDataIdentifier(selection: MarketDataSelection): MarketDataIdentifier
  def identifierFor(selection: MarketDataSelection, version: Option[MarketDataVersion]) = version.fold(
    v => MarketDataIdentifier(selection, v), latestMarketDataIdentifier(selection))

  def latestObservationDayForMarketDataSet(marketDataSet: MarketDataSet): Option[Day]
  def latestObservationDayFor(pricingGroup: PricingGroup, marketDataType: MarketDataTypeName): Option[Day]

  def latestPricingGroupVersions: Map[PricingGroup, Int]

  def latestSnapshot(pricingGroup: PricingGroup): Option[SnapshotID]

  def marketData(from: Day, to: Day, marketDataType: MarketDataTypeName, marketDataSet: MarketDataSet): Map[TimedMarketDataKey, VersionedMarketData]

  def availableMarketDataTypes(marketDataIdentifier: MarketDataIdentifier): List[MarketDataType]

  def marketDataTypes:MarketDataTypes

  def observationDaysByExcel(): Map[String, Set[Day]]
  def observationDaysByPricingGroup(): Map[PricingGroup, Set[Day]]

  def queryLatest(selection: MarketDataSelection, marketDataType: MarketDataTypeName,
    observationDays: Option[Set[Option[Day]]] = None, observationTimes: Option[Set[ObservationTimeOfDay]] = None,
    marketDataKeys: Option[Set[MarketDataKey]] = None): List[(TimedMarketDataKey, MarketData)] = {

    query(latestMarketDataIdentifier(selection), marketDataType, observationDays, observationTimes, marketDataKeys)
  }

  def query(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataTypeName,
            observationDays: Option[Set[Option[Day]]] = None, observationTimes: Option[Set[ObservationTimeOfDay]] = None,
            marketDataKeys: Option[Set[MarketDataKey]] = None): List[(TimedMarketDataKey, MarketData)]

  def queryForObservationDayAndMarketDataKeys(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataTypeName): List[TimedMarketDataKey]

  def readLatest(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey): Option[VersionedMarketData]

  def save(marketDataSetToData: MultiMap[MarketDataSet, MarketDataEntry]): SaveResult

  def save(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey, marketData: MarketData): Int

  def saveAll(marketDataSet: MarketDataSet, observationPoint: ObservationPoint, data: Map[MarketDataKey, MarketData]): SaveResult

  def update(marketDataSetToData: MultiMap[MarketDataSet, MarketDataUpdate]): SaveResult

  def snapshot(marketDataIdentifier: MarketDataIdentifier, snapshotType:SnapshotType): SnapshotID

  def snapshots(): List[SnapshotID]
  
  def snapshots(observationDay : Option[Day]): List[SnapshotID] = snapshots().filter {
      snapshotID =>
        observationDay match {
          case Some(day) => day <= snapshotID.snapshotDay
          case None => true
        }
    }

  def snapshotsByMarketDataSelection(): MultiMap[MarketDataSelection, SnapshotIDLabel]

  def snapshotFromID(snapshotID: Int): Option[SnapshotID]
  def snapshotFromID(snapshotID: Option[Int]): Option[SnapshotID] = snapshotID.map(snapshotFromID(_)).flatOpt

  def sourcesFor(marketDataSet: MarketDataSet): List[MarketDataSource]
  def sourcesFor(pricingGroup: PricingGroup): List[MarketDataSource]
}

case class SaveResult(maxVersion: Int, anythingChanged: Boolean, affectedObservationDays: Option[List[Day]] = None)

object SaveResult {
  val Null = SaveResult(0, false, None)
}

case class VersionedMarketData(version: Int, data: Option[MarketData])

object VersionedMarketData {
  def apply(version: Int, marketData: MarketData): VersionedMarketData =
    if (marketData.size == 0) VersionedMarketData(version, None) else VersionedMarketData(version, Option(marketData))

  val Delete = Extractor.when[VersionedMarketData](_.data.isEmpty)
  val Save = Extractor.when[VersionedMarketData](_.data.isDefined)
}

case class MarketDataEntry(observationPoint: ObservationPoint, key: MarketDataKey, data: MarketData, tag: Option[String] = None) {
  val dataType = key.typeName

  def isEmpty = data.size == 0 // key.castRows(data, ReferenceDataLookup.Null).isEmpty

  def toSave(existingData: Option[VersionedMarketData]) = !isEmpty option(MarketDataUpdate(timedKey, Some(data), existingData, tag))

  def toUpdate(existingData: Option[VersionedMarketData]) = toSave(existingData).getOrElse(MarketDataUpdate(timedKey, None, existingData, tag))

  def timedKey = TimedMarketDataKey(observationPoint, key)

  def dataIdFor(marketDataSet: MarketDataSet, types: MarketDataTypes) = MarketDataID(timedKey, marketDataSet, types)
  def copyDay(day: Day) = copy(observationPoint.copyDay(day))
}

case class MarketDataUpdate(timedKey: TimedMarketDataKey, data: Option[MarketData], existingData: Option[VersionedMarketData],
                            tag: Option[String] = None) {

  def observationPoint = timedKey.observationPoint
  def marketDataKey = timedKey.key
  def dataIdFor(marketDataSet: MarketDataSet, types: MarketDataTypes) = MarketDataID(timedKey, marketDataSet, types)
}

class MarketDataSnapshots(db: DBTrait[RichResultSetRow]) {

  def versionForSnapshot(snapshotID: SnapshotID) = {
    db.queryWithOneResult("select commitid from MarketDataSnapshots where snapshotid = :id", Map("id" -> snapshotID.id)) {
      rs => rs.getInt("commitid")
    }.get
  }

  def latestSnapshot(pricingGroup: PricingGroup): Option[SnapshotID] = {
    db.queryWithOneResult(
         select("*")
           from("MarketDataSnapshots")
          where("marketDataSelection" eql PersistAsBlob(MarketDataSelection(Some(pricingGroup))))
        orderBy("commitID" desc))
    { SnapshotID(_) }
  }

  def snapshot(version: Int, marketDataSelection: MarketDataSelection, snapshotType: SnapshotType) = {
    val existingSnapshot = if (snapshotType == SnapshotType.Manual) None else { //always create a snapshot when manual
      db.queryWithOneResult((select("*") from "MarketDataSnapshots" where (("commitid" eql version)
        and ("marketDataSelection" eql PersistAsBlob(marketDataSelection))
        and ("snapshotType" eql snapshotType.name)))) { SnapshotID(_) }
    }


    existingSnapshot match {
      case Some(ss) => (ss, false)
      case None => {
        val timestamp = new Timestamp()
        val params = Map(
          "commitid" -> version,
          "snapshotTime" -> timestamp, "marketDataSelection" -> StarlingXStream.write(marketDataSelection),
            "snapshotType" -> snapshotType.name)
        var id: Option[Long] = None
        db.inTransaction(writer => id = Some(writer.insertAndReturnKey(
          "MarketDataSnapshots", "snapshotid", params,
          Some(List("snapshotTime", "commitid", "marketDataSelection", "snapshotType")))))
        val ss = SnapshotID(id.get.toInt, timestamp, marketDataSelection, snapshotType, version)
        (ss, true)
      }

    }
  }

  def snapshots(): List[SnapshotID] = {
    db.queryWithResult("select * from MarketDataSnapshots order by snapshotID desc", Map()) {
      rs => snapshotIDFromResultSetRow(rs)
    }
  }

  private def snapshotIDFromResultSetRow(rs: RichResultSetRow) = SnapshotID(rs)

  def snapshotsByMarketDataSelection(): MultiMap[MarketDataSelection, SnapshotIDLabel] = {
    snapshots().groupBy(_.marketDataSelection).map {
      case (selection, snapshots) => selection -> snapshots.map(_.label).sortWith(_ > _)
    }
  }

  def latestSnapshotFor(selection: MarketDataSelection): Option[SnapshotIDLabel] = {
    snapshots().filter(_.marketDataSelection == selection).optMaxBy(_.version).map(_.label)
  }

  def snapshotFromID(snapshotID: Int): Option[SnapshotID] = {
    db.queryWithOneResult("""
    select *
    from MarketDataSnapshots
    where
      snapshotID = :snapshotID
    """, Map("snapshotID" -> snapshotID))(SnapshotID(_))
  }
}
