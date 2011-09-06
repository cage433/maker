package starling.db

import starling.dbx.QueryBuilder._
import starling.quantity.Percentage
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.richdb.RichResultSetRow
import starling.utils.sql._
import starling.gui.api._
import starling.utils.cache.CacheFactory
import starling.daterange._
import starling.calendar.Clock

import starling.instrument.utils.StarlingXStream
import starling.utils.Pattern._
import java.lang.String
import java.util.concurrent.atomic.AtomicInteger
import starling.utils._
import collection.immutable.{Map, TreeMap}
import collection.mutable.{SetBuilder, ListBuffer, HashSet => MSet}
import scalaz._
import Scalaz._
import collection.mutable.{ListBuffer, HashSet => MSet}
import collection.immutable.{Iterable, Map, TreeMap}
import starling.pivot.{PivotQuantity, PivotEdits, PivotTableDataSource, Field => PField}
import starling.dbx._

//import starling.props.Props.VarReportEmailFrom

// TODO [07 Sep 2010] move me somewhere proper
case class RelativeImpliedVolData(vols: Map[DateRange, Map[Double, Percentage]]) {
  def absolute(prices: PriceData): ImpliedVolData = {
    val data = vols.flatMap {
      case (period, row) => {
        // lame hack - why, trinity? WHY???
        val exerciseDay = period.firstDay - 2
        val strike: Double = prices.prices(exerciseDay).quantityValue.get.value
        row.map((tuple: (Double, Percentage)) => {
          (ImpliedVolEntryKey(period, strike + tuple._1, exerciseDay), tuple._2)
        })
      }
    }
    ImpliedVolData(TreeMap.empty[ImpliedVolEntryKey, Percentage](ImpliedVolEntryKey) ++ data)
  }
}

/**
 * Thrown when there is no market data available for a particular day
 */
case class NoMarketDataForDayException(observationDay: Day, m: String) extends Exception(m)

trait MarketDataSource {
  self =>
  def read(day: Day): Map[(Day, Day, MarketDataType), List[MarketDataEntry]]

  def asserting(): MarketDataSource = new MarketDataSource {
    def read(day: Day) = {
      val map: Map[(Day, Day, MarketDataType), List[MarketDataEntry]] = self.read(day)
      val result = map.updateIt(r => duplicateTimedKeys(r).require(_.isEmpty, "source: %s produced duplicate 'timed' keys: " % self))
      result
    }

    def duplicateTimedKeys(map: Map[(Day, Day, MarketDataType), List[MarketDataEntry]]): List[TimedMarketDataKey] =
      map.values.flatMap(duplicateTimedKeys).toList
  }

  protected def containsDistinctTimedKeys(entries: List[MarketDataEntry]): Boolean = duplicateTimedKeys(entries).isEmpty

  protected def duplicateTimedKeys(entries: List[MarketDataEntry]) = entries.map(_.timedKey).duplicates

  def description: List[String] = Nil
}

class AdaptingMarketDataSource(adaptee: MarketDataSource) extends MarketDataSource {
  def read(day: Day) = adaptee.read(day)
}

case class MarketDataSet(name: String)

object MarketDataSet extends StarlingEnum(classOf[MarketDataSet], (m: MarketDataSet) => m.name) {
  val excelPrefix = "Excel:"

  def excel(name: String) = {
    if (name == "Official:Metals") {
      MarketDataSet.ManualMetals
    } else {
      MarketDataSet(excelPrefix + name)
    }
  }

  def fromExcel(marketDataSet: MarketDataSet) = {
    if (marketDataSet.name.startsWith(excelPrefix)) {
      Some(marketDataSet.name.stripPrefix(excelPrefix))
    } else {
      None
    }
  }

  val LIM = MarketDataSet("LIM") //Oil and Metals VAR
  val System = MarketDataSet("System")
  val Crude = MarketDataSet("Crude")
  val LondonDerivatives = MarketDataSet("London Derivatives")
  val GasolineRoW = MarketDataSet("Gasoline RoW")
  val BarryEckstein = MarketDataSet("Barry Eckstein")
  val LondonDerivativesOptions = MarketDataSet("London Derivatives Options")
  val LimMetals = MarketDataSet("LimMetals") //Refined Metals
  val ManualMetals = MarketDataSet("ManualMetals")
  val Starling = MarketDataSet("Starling")
  val TrinityDiscountFactorCSV = MarketDataSet("TrinityDiscountFactorCSV")
  val Neptune = MarketDataSet("TrinityDiscountFactorCSV")

  /*
   val TrinityLive = MarketDataSet("Trinity/Live")
   val GalenaLive = MarketDataSet("Galena/Live")
   val GalenaFullCurve = MarketDataSet("Galena/FullCurve")
   val VarMetalsFreight = MarketDataSet("Var:Metals+Freight")
   val VarGalenaLondon = MarketDataSet("Var:GalenaLondon")
   val VarGalenaOil= MarketDataSet("Var:GalenaOil")
  */
}

object MarketDataStore {

  import MarketDataSet._

  val pricingGroupsDefinitions = Map[PricingGroup, List[MarketDataSet]](
    PricingGroup.Metals -> List(ManualMetals, LimMetals, TrinityDiscountFactorCSV, Neptune),
    PricingGroup.LimOnly -> List(LIM),
    PricingGroup.System -> List(Starling, LIM, System),
    PricingGroup.Crude -> List(Starling, LIM, Crude),
    PricingGroup.LondonDerivatives -> List(Starling, LondonDerivatives, LIM),
    PricingGroup.GasolineRoW -> List(Starling, GasolineRoW, LIM),
    PricingGroup.BarryEckstein -> List(Starling, BarryEckstein, System, LIM),
    PricingGroup.LondonDerivativesOptions -> List(Starling, LondonDerivativesOptions, System, LIM),
    PricingGroup.Starling -> List(Starling)
  )

  val orphanedPricingGroups = (PricingGroup.values \\ Desk.pricingGroups.intersect(pricingGroupsDefinitions.keys.toList))
    .desire(_.isEmpty, "Orphaned Pricing Groups:")

  val orphanedMarketDataSets = (MarketDataSet.values \\ Desk.pricingGroups.flatMap(pricingGroupsDefinitions).distinct)
    .desire(_.isEmpty, "Orphaned Market Data Sets:")

  val manuallyEditableMarketDataSets = Set(ManualMetals, Starling)

  require(pricingGroupsDefinitions.keySet == PricingGroup.values.toSet, "Need to add PricingGroups in two places")
  val pricingGroups = pricingGroupsDefinitions.keySet.toList

  def pricingGroupForName(name: String): PricingGroup = pricingGroups.find(_.name == name).get

  def editableMarketDataSetFor(pricingGroup: PricingGroup) = {
    pricingGroupsDefinitions.get(pricingGroup).flatMap {
      sets => (manuallyEditableMarketDataSets & sets.toSet).headOption
    }
  }

  def applyOverrideRule(marketDataType: MarketDataType, allDataForKeyAndDay: List[Map[PField, Any]]): List[Map[PField, Any]] = {
    val dataAsMaps: Map[Map[PField, Any], Map[PField, Any]] = Map() ++ allDataForKeyAndDay.map(marketDataType.splitByFieldType(_))
    val m = scala.collection.mutable.HashMap[Map[PField, Any], Map[PField, Any]]()
    dataAsMaps.foreach {
      case (k, v) => {
        m(k) = v
      }
    }
    m.map {
      case (k, v) => k ++ v
    }.toList
  }
}

/**
 * Wraps a data type to provide version information.
 */

trait MarketDataStore {
  val pricingGroupsDefinitions = MarketDataStore.pricingGroupsDefinitions
  val pricingGroups = MarketDataStore.pricingGroups

  def excelDataSets: List[String]

  def importData(marketDataSelection: MarketDataSelection, observationDay: Day): SaveResult

  def importFor(observationDay: Day, marketDataSets: MarketDataSet*): SaveResult

  def latest(selection: MarketDataSelection): Int

  def latestExcelVersions: Map[String, Int]

  def latestMarketDataIdentifier(selection: MarketDataSelection): MarketDataIdentifier
  def identifierFor(selection: MarketDataSelection, version: Option[MarketDataVersion]) = version.fold(
    v => MarketDataIdentifier(selection, v), latestMarketDataIdentifier(selection))

  def latestObservationDayForMarketDataSet(marketDataSet: MarketDataSet): Option[Day]

  def latestPricingGroupVersions: Map[PricingGroup, Int]

  def latestSnapshot(pricingGroup: PricingGroup, observationDay: Day): Option[SnapshotID]

  def marketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)]

  def marketDataTypes(marketDataIdentifier: MarketDataIdentifier): List[MarketDataType]

  def observationDaysByExcel(): Map[String, Set[Day]]

  def observationDaysByPricingGroup(): Map[PricingGroup, Set[Day]]

  def pivot(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): PivotTableDataSource

  def pivot(selection: MarketDataSelection, marketDataType: MarketDataType): PivotTableDataSource

  def query(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]] = None, observationTimes: Option[Set[ObservationTimeOfDay]] = None,
            marketDataKeys: Option[Set[MarketDataKey]] = None): List[(TimedMarketDataKey, MarketData)]

  def queryForObservationDayAndMarketDataKeys(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): List[TimedMarketDataKey]

  def readLatest[T <: MarketData](marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey): Option[T]

  def save(marketDataSetToData: Map[MarketDataSet, Iterable[MarketDataEntry]]): SaveResult

  def save(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey, marketData: MarketData): Int

  def saveAll(marketDataSet: MarketDataSet, observationPoint: ObservationPoint, data: Map[MarketDataKey, MarketData]): SaveResult

  def snapshot(marketDataSelection: MarketDataSelection, doImport: Boolean, observationDay: Day): Option[SnapshotID]

  def snapshots(): List[SnapshotID]

  def snapshotsByMarketDataSelection(): Map[MarketDataSelection, List[SnapshotIDLabel]]

  def snapshotFromID(snapshotID: Int): Option[SnapshotID]

  def snapshotFromID(snapshotID: Option[Int]): Option[SnapshotID] = snapshotID.map(snapshotFromID(_)).flatOpt

  def sourceFor(marketDataSet: MarketDataSet): Option[MarketDataSource]

  def sourcesFor(pricingGroup: PricingGroup): List[MarketDataSource]
}

case class SaveResult(maxVersion: Int, anythingChanged: Boolean, affectedObservationDays: Option[List[Day]] = None)

case class VersionedMarketData(timestamp: Timestamp, version: Int, data: Option[MarketData])

object VersionedMarketData {
  val Delete = Extractor.when[VersionedMarketData](_.data.isEmpty)
  val Save = Extractor.when[VersionedMarketData](_.data.isDefined)
}

case class MarketDataEntry(observationPoint: ObservationPoint, key: MarketDataKey, data: MarketData, tag: Option[String] = None) {
  val dataType = key.dataType

  def isEmpty = key.castRows(data).isEmpty

  def toSave(existingData: Option[VersionedMarketData]) = if (isEmpty) None else Some(MarketDataUpdate(timedKey, Some(data), existingData, tag))

  def toUpdate(existingData: Option[VersionedMarketData]) = toSave(existingData).getOrElse(MarketDataUpdate(timedKey, None, existingData, tag))

  def timedKey = TimedMarketDataKey(observationPoint, key)

  def dataIdFor(marketDataSet: MarketDataSet) = MarketDataID(timedKey, marketDataSet)
}

case class MarketDataUpdate(timedKey: TimedMarketDataKey, data: Option[MarketData], existingData: Option[VersionedMarketData],
                            tag: Option[String] = None) {
  def observationPoint = timedKey.observationPoint

  def marketDataKey = timedKey.key

  def dataIdFor(marketDataSet: MarketDataSet) = MarketDataID(timedKey, marketDataSet)

  def indexedData(marketDataSet: MarketDataSet) = dataIdFor(marketDataSet) → data
}

trait MdDB {
  def checkIntegrity(): Unit

  def readAll(): Unit

  def marketDataSetNames(): List[String]

  def observationDaysFor(marketDataSets: List[MarketDataSet]): List[Option[Day]]

  def excelObservationDays(): List[(String, Option[Day])]

  def latestVersionForMarketDataSets(): Map[MarketDataSet, Int]

  def latestExcelVersions(): Map[String, Int]

  def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet): SaveResult

  def maxVersionForMarketDataSetNames(names: List[String]): Option[Int]

  def marketDataTypes(version: Int, mds: List[MarketDataSet]): List[MarketDataType]

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)]

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType): List[TimedMarketDataKey]

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)]

  def readLatest(id: MarketDataID): Option[VersionedMarketData]
}

//class FastMdDB(db: DBTrait[RichResultSetRow]) extends MdDB {
//
//  //read early:
//  //all observation days
//  //all Key1 (mds, time, key)
//  //all Key2
//
//
//  //will have (Day) -> (MDS,ObsTime,MarketDataKey) -> (SecondKey) -> Version -> (Option[Value],Timestamp,User)
//
//
//  def checkIntegrity():Unit = {}
//  def readAll():Unit  = {}
//
//  def marketDataSetNames():List[String]  = {}// "select distinct marketDataSet from Key2"
//
//  // "select observationDay from Values join Key1 on .. where mds =
//  def observationDaysFor(marketDataSets:List[MarketDataSet]):List[Option[Day]]  = {}
//  def excelObservationDays():List[(String,Option[Day])] = {}
//
//   //"select max(commitid), mds from Values join on Key1 ,,,"
//  def latestVersionForMarketDataSets():Map[MarketDataSet,Int]
//  def latestExcelVersions():Map[String, Int]
//  def maxVersionForMarketDataSetNames(names:List[String]):Option[Int] //remove?
//
//  class Cursor(version:Int, mds:List[MarketDataSet]) {
//    //
//  }
//
//  def marketDataTypes(version:Int, mds:List[MarketDataSet]) : List[MarketDataType]
//  def queryForObservationDayAndMarketDataKeys(version:Int, mds:List[MarketDataSet], marketDataType: MarketDataType): List[TimedMarketDataKey]
//  def query(version:Int, mds:List[MarketDataSet], marketDataType: MarketDataType,
//            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
//            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)]
//
//  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)]
//  def readLatest(id:MarketDataID):Option[VersionedMarketData]
//
//  def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet):(Boolean,Int)
//
//}

class SlowMdDB(db: DBTrait[RichResultSetRow]) extends MdDB with Log {

  private val currentVersion = new AtomicInteger(
    db.queryWithOneResult[Int]("SELECT MAX(version) as version from MarketData")(_.getInt("version")).getOrElse(1))

  def checkIntegrity() {
    val q = (select("observationDay, observationTime, marketDataSet, marketDataType, marketDataKey")
      from "MarketData"
      where ("childVersion" isNull)
      groupBy ("observationDay, observationTime, marketDataSet, marketDataType, marketDataKey")
      having ("COUNT(marketDataKey)" gt 1))

    val duplicates = db.queryWithResult(q) {
      rs => {
        println("!!! :" + rs)
        rs.getString("marketDataSet")
      }
    }

    assert(duplicates.isEmpty, "The MDS is corrupt\n" + q)
  }

  def readAll() {
    log.infoWithTime("Reading all market data") {

      val errors = scala.collection.mutable.HashSet[String]()
      var month: Option[Month] = None
      db.query((select("*") from "MarketData") orderBy Desc("observationDay")) {
        rs => {
          try {
            val observationDay = rs.getDayOption("observationDay")
            //marketDataSet
            val marketDataType = rs.getObject[MarketDataType]("marketDataType")
            val key = rs.getObject[MarketDataKey]("marketDataKey")
            val data = rs.getObjectOption[Any]("data").map(key.unmarshallDB(_))
            observationDay match {
              case None =>
              case Some(day) => {
                month match {
                  case None => month = Some(day.containingMonth)
                  case Some(m) => {
                    if (day.containingMonth != m) {
                      println("Read market data for " + m)
                      month = Some(day.containingMonth)
                    }
                  }
                }
              }
            }
            None
          } catch {
            case e: Exception => {
              if (!errors.contains(e.getMessage)) {
                e.printStackTrace
                errors += (e.getMessage)
              }
            }
          }
        }
      }
      println(errors.size + " errors")
      errors.foreach {
        e => println("  " + e)
      }
    }
  }

  def marketDataSetNames(): List[String] = {
    db.queryWithResult("select distinct marketDataSet from MarketData order by marketDataSet", Map()) {
      rs => rs.getString("marketDataSet")
    }
  }

  def observationDaysFor(marketDataSets: List[MarketDataSet]): List[Option[Day]] = {
    db.queryWithResult((select("distinct observationDay") from "MarketData" where ("marketDataSet" in marketDataSets.map(_.name)))) {
      rs => rs.getDayOption("observationDay")
    }
  }

  def excelObservationDays(): List[(String, Option[Day])] = {
    db.queryWithResult((select("distinct marketDataSet, observationDay") from "MarketData" where ("marketDataSet" like "Excel:%"))) {
      rs => (rs.getString("marketDataSet").stripPrefix(MarketDataSet.excelPrefix), rs.getDayOption("observationDay"))
    }
  }

  def latestVersionForMarketDataSets(): Map[MarketDataSet, Int] = {
    Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet not like 'Excel:%' group by marketDataSet ", Map()) {
      rs => MarketDataSet(rs.getString("marketDataSet")) -> rs.getInt("m")
    }
  }

  def latestExcelVersions(): Map[String, Int] = {
    Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet like 'Excel:%' group by marketDataSet ", Map()) {
      rs => rs.getString("marketDataSet").stripPrefix("Excel:") → rs.getInt("m")
    }
  }

  def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet): SaveResult = {
    var update = false
    var innerMaxVersion = 0

    db.inTransaction(dbWriter =>
      data.map {
        action =>
          updateIt(dbWriter, action.dataIdFor(marketDataSet), action.existingData, action.data).foreach {
            result =>
              if (result._1) update = true
              innerMaxVersion = scala.math.max(innerMaxVersion, result._2)
          }
      }
    )
    SaveResult(innerMaxVersion, update)
  }

  private def updateIt(dbWriter: DBWriter, id: MarketDataID, existingData: Option[VersionedMarketData], maybeData: Option[MarketData]) = {
    val timestamp = Clock.timestamp

    (existingData, maybeData) match {
      case (None, None) => None
      case (Some(VersionedMarketData(timestamp, v, Some(d))), Some(data)) if d == data => Some((false, v))
      case (_, x) => {
        val valueForDataColumn = x.map(v => StarlingXStream.write(v.marshall)).getOrElse(null) // null means delete
        var result: Option[Int] = None
        //timer {
        // insert new row into table, returning new version ID.

        val nextVersion = currentVersion.incrementAndGet

        if (existingData.isDefined) {
          // update previous version to point to new version ID.
          dbWriter.queryWithNoResults(
            QueryBuilder.update("MarketData")
              set ("childVersion" eql nextVersion)
              where ("version" eql existingData.get.version))
        }

        dbWriter.withIdentityInsert("MarketData") {
          val values = id.conditions + ("timestamp" → timestamp) + ("data" → valueForDataColumn) + ("version" → nextVersion)

          dbWriter.insert("MarketData", values)
        }

        result = Some(nextVersion.toInt)
        //}
        Some((true, result.get))
      }
    }
  }


  def maxVersionForMarketDataSetNames(names: List[String]): Option[Int] = {
    db.queryWithOneResult("select max(version) m from MarketData where marketDataSet in (:mds)", Map("mds" → names)) {
      _.getInt("m")
    }
  }

  def marketDataTypes(version: Int, mds: List[MarketDataSet]): List[MarketDataType] = {
    queryForMarketDataIdentifier(
      version, mds,
      "distinct marketDataType",
      None,
      rs => rs.getObject[MarketDataType]("marketDataType")
    )
  }

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)] = {
    val query = (
      select("observationDay, observationTime, marketDataKey, data, timestamp, version")
        from "MarketData"
        where (("marketDataSet") eql marketDataSet.name)
        and ("observationDay" gte from)
        and ("observationDay" lte to)
        and ("marketDataType" eql PersistAsBlob(marketDataType))
        and ("childVersion" isNull)
      )
    val results = db.queryWithResult(query) {
      rs => {
        val key: MarketDataKey = rs.getObject[MarketDataKey]("marketDataKey")
        (TimedMarketDataKey(observationPoint(rs), key), whatIsThis(key, rs))
      }
    }
    val check = results.groupBy(e => (e._1, e._2))
    check.foreach {
      case (k, v) if v.size > 1 => {
        log.error("MDS has gotten corrupt: " +(marketDataSet, marketDataType, k, v))
        throw new Exception("Duplicate data in MDS for " +(marketDataSet, marketDataType, k, v))
      }
      case _ =>
    }
    results
  }

  private def observationPoint(rs: ResultSetRow) =
    ObservationPoint(rs.getDay("observationDay"), ObservationTimeOfDay.fromName(rs.getString("observationTime")))

  private def queryForMarketDataIdentifier[T](
                                               version: Int,
                                               mds: List[MarketDataSet],
                                               selectQuery: String,
                                               clause: Option[Clause],
                                               f: RichResultSetRow => T) = {
    val versionClause = (("version" lte version) and ((("childVersion") isNull) or ("childVersion" gt version)))
    val query = (
      select(selectQuery)
        from "MarketData"
        where ((("marketDataSet" in mds.map(_.name)) and versionClause) andMaybe clause)
      )
    db.queryWithResult(query) {
      rs => f(rs)
    }
  }

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType): List[TimedMarketDataKey] = {
    db.queryWithResult(
      (select("distinct observationTime, observationDay, marketDataKey")
        from ("MarketData")
        where typeAndSnapshotClauses(version, mds, marketDataType))) {
      rs => {
        val observationPoint = {
          val observationTime = ObservationTimeOfDay.fromName(rs.getString("observationTime"))
          rs.getDayOption("observationDay") match {
            case Some(day) => ObservationPoint(day, observationTime)
            case None => ObservationPoint.RealTime
          }
        }
        val marketDataKey = rs.getObject[MarketDataKey]("marketDataKey")

        TimedMarketDataKey(observationPoint, marketDataKey)
      }
    }
  }

  private def typeAndSnapshotClauses(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType) = {
    (("marketDataSet" in mds.map(_.name))
      and (("version" lte version) and ((("childVersion") isNull) or ("childVersion" gt version)))
      and ("marketDataType" eql LiteralString(StarlingXStream.write(marketDataType))))
  }

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {

    val observationDayClause = {
      observationDays.map {
        days => {
          val actualDays = days.toList.somes
          val inClause = In(Field("observationDay"), actualDays)
          if (days.contains(None)) {
            (inClause or ("observationDay" isNull))
          } else {
            inClause
          }
        }
      }
    }
    val observationTimeClause = observationTimes.map(times => In(Field("observationTime"), times.map(_.name)))
    val keyClause = marketDataKeys.map(keys => In(Field("marketDataKey"), keys.map(StarlingXStream.write(_))))

    val query = (
      select("*")
        from "MarketData"
        where typeAndSnapshotClauses(version, mds, marketDataType) :: observationDayClause.toList :::
        observationTimeClause.toList ::: keyClause.toList
        orderBy "observationDay, marketDataKey, marketDataSet".desc
      )

    val data = new ListBuffer[(TimedMarketDataKey, MarketData)]()
    var latestKey: Option[(String, Option[Day], MarketDataKey)] = None
    val latestValues = new scala.collection.mutable.HashMap[String, (MarketData)]()

    def addEntry(timeOfDay: String, day: Option[Day], key: MarketDataKey) {
      val allDataForKeyAndDay: List[Map[PField, Any]] = mds.reverse.flatMap(mds => {
        latestValues.get(mds.name).toList.flatMap(data => key.castRows(data))
      })
      val rowsFromOneMap = MarketDataStore.applyOverrideRule(marketDataType, allDataForKeyAndDay)
      if (rowsFromOneMap.nonEmpty) {
        val marketData = marketDataType.createValue(rowsFromOneMap)
        val observationPoint = day match {
          case None => ObservationPoint.RealTime
          case Some(day) => ObservationPoint(day, ObservationTimeOfDay.fromName(timeOfDay))
        }
        data.append(TimedMarketDataKey(observationPoint, key) → marketData)
      }
    }

    //For each row we record the marketData for each marketDataSet
    //When the 'key' changes we choose the 'best' marketDataSet avalaible
    //where best is the first in the marketDataSets list
    db.query(query) {
      rs => {
        val observationDay = rs.getDayOption("observationDay")
        val observationTime = rs.getString("observationTime")
        val marketDataKey = rs.getObject[MarketDataKey]("marketDataKey")
        val data = rs.getObjectOption[Any]("data")
        val optionMarketData = data.map(marketDataKey.unmarshallDB(_))
        val marketDataSet = rs.getString("marketDataSet")

        val key = (observationTime, observationDay, marketDataKey)

        optionMarketData.map {
          marketData =>
            latestKey match {
              case None => {
                latestKey = Some(key)
                latestValues.clear()
                latestValues(marketDataSet) = marketData
              }
              case Some(`key`) => {
                latestValues(marketDataSet) = marketData
              }
              case Some((timeOfDay, day, k)) => {
                addEntry(timeOfDay, day, k)
                latestKey = Some(key)
                latestValues.clear()
                latestValues(marketDataSet) = marketData
              }
            }
        }
      }
    }

    latestKey match {
      case Some((time, day, k)) => addEntry(time, day, k)
      case None =>
    }

    data.toList
  }


  def readLatest(id: MarketDataID): Option[VersionedMarketData] = {
    def queryForVersion(key: MarketDataID): Query = {
      import QueryBuilder._

      (select("timestamp, version, data")
        from ("MarketData")
        where (conditions(key)
        and ("childVersion" isNull)))
      //    and version.map("version" eql _).getOrElse("childVersion" isNull)))
    }

    def conditions(key: MarketDataID): Clause = conditionsFromMap(key.conditions)

    def conditionsFromMap(conditions: Map[String, Any]) = {
      conditions.map {
        case (k, null) => k isNull
        case (k, v) => k eql literal(v)
      }.reduceLeft((a: Clause, b: Clause) => a and b)
    }

    def literal(v: Any): Any = v match {
      case s: String => LiteralString(s)
      case _ => v
    }

    db.queryWithOneResult(queryForVersion(id)) {
      rs => whatIsThis(id.subTypeKey, rs)
    }
  }

  private def whatIsThis(key: MarketDataKey, rs: ResultSetRow) = {
    new VersionedMarketData(rs.getTimestamp("timestamp"), rs.getInt("version"),
      rs.getObjectOption[Any]("data").map(key.unmarshallDB(_)))
  }

}

class MarketDataTags(db: DBTrait[RichResultSetRow]) {

  def versionForSnapshot(snapshotID: SnapshotID) = {
    db.queryWithOneResult("select version from MarketDataTag where snapshotid = :id", Map("id" -> snapshotID.id)) {
      rs => rs.getInt("version")
    }.get
  }

  def latestSnapshot(pricingGroup: PricingGroup, observationDay: Day): Option[SnapshotID] = {
    db.queryWithOneResult("""
    select *
    from MarketDataTag
    where
      pricingGroup = :pricingGroup
      and observationDay = :observationDay
    order by snapshotTime desc
    """, Map("pricingGroup" -> StarlingXStream.write(pricingGroup), "observationDay" -> observationDay)) {
      rs => SnapshotID(rs)
    }
  }

  def snapshot(version: Int, marketDataSelection: MarketDataSelection, observationDay: Day) = {
    import QueryBuilder._
    val optSnapshot = db.queryWithOneResult((select("*") from "MarketDataTag" where (("version" eql version)
      and ("marketDataSelection" eql LiteralString(StarlingXStream.write(marketDataSelection)))
      and ("observationDay" eql observationDay)))) {
      rs => SnapshotID(rs)
    }

    optSnapshot match {
      case Some(ss) => (ss, false)
      case None => {
        val timestamp = new Timestamp()
        val params = Map("snapshotTime" -> timestamp, "version" -> version, "marketDataSelection" -> StarlingXStream.write(marketDataSelection), "observationDay" -> observationDay)
        var id: Option[Long] = None
        db.inTransaction(writer => id = Some(writer.insertAndReturnKey("MarketDataTag", "snapshotid", params, Some(List("snapshotTime", "version", "marketDataSelection", "observationDay")))))
        val ss = SnapshotID(observationDay, id.get.toInt, timestamp, marketDataSelection, version)
        (ss, true)
      }

    }
  }

  def snapshots(): List[SnapshotID] = {
    db.queryWithResult("select * from MarketDataTag order by snapshotID desc", Map()) {
      rs => snapshotIDFromResultSetRow(rs)
    }
  }

  private def snapshotIDFromResultSetRow(rs: RichResultSetRow) = SnapshotID(rs)

  def snapshotsByMarketDataSelection(): Map[MarketDataSelection, List[SnapshotIDLabel]] = {
    snapshots().groupBy(_.marketDataSelection).map {
      case (selection, snapshots) => selection -> snapshots.map(_.label).sortWith(_ > _)
    }
  }

  def latestSnapshotFor(selection: MarketDataSelection): Option[SnapshotIDLabel] = {
    snapshots().filter(_.marketDataSelection == selection).optMaxBy(_.id).map(_.label)
  }

  def snapshotFromID(snapshotID: Int): Option[SnapshotID] = {
    db.queryWithOneResult("""
    select *
    from MarketDataTag
    where
      snapshotID = :snapshotID
    """, Map("snapshotID" -> snapshotID))(SnapshotID(_))
  }

}

// TODO [12 May 2011] move me somewhere proper
class DBMarketDataStore(db: MdDB,
                        tags: MarketDataTags,
                        val marketDataSources: Map[MarketDataSet, MarketDataSource],
                        broadcaster: Broadcaster) extends MarketDataStore with Log {

  def this(db: DBTrait[RichResultSetRow], marketDataSources: Map[MarketDataSet, MarketDataSource],
           broadcaster: Broadcaster = Broadcaster.Null) = this (new SlowMdDB(db), new MarketDataTags(db), marketDataSources, broadcaster)

  db.checkIntegrity()

  val importer = new MarketDataImporter(this)
  val pivotCache = CacheFactory.getCache("MarketDataStore.pivotCache")

  def readAll() {
    snapshotsByMarketDataSelection() //read MarketDataTag table
    db.readAll()
  }

  def sourceFor(marketDataSet: MarketDataSet) = marketDataSources.get(marketDataSet)

  def sourcesFor(pricingGroup: PricingGroup) = marketDataSets(MarketDataSelection(Some(pricingGroup))).flatMap(sourceFor(_))

  private var excelDataSetsCache = {
    scala.collection.mutable.ArrayBuffer[String]() ++ db.marketDataSetNames().flatMap {
      name => {
        if (name.startsWith(MarketDataSet.excelPrefix)) {
          Some(name.stripPrefix(MarketDataSet.excelPrefix))
        } else {
          None
        }
      }
    }
  }

  private val observationDaysByPricingGroupCache = {
    new scala.collection.mutable.HashMap() ++ pricingGroupsDefinitions.mapValues {
      (marketDataSets) => {
        val list = db.observationDaysFor(marketDataSets)
        new scala.collection.mutable.HashSet() ++ list.somes
      }
    }
  }

  private val observationDaysByExcelCache = {
    val list: scala.List[(String, Option[Day])] = db.excelObservationDays()
    new scala.collection.mutable.HashMap() ++ list.groupBy(_._1).mapValues(new MSet() ++ _.flatMap(_._2.toList))
  }

  def excelDataSets: List[String] = excelDataSetsCache.toList

  def pricingGroupForName(name: String) = MarketDataStore.pricingGroupForName(name)

  def versionForSnapshot(snapshotID: SnapshotID) = {
    tags.versionForSnapshot(snapshotID)
  }

  def validate(reader: MarketDataReader): MarketDataReader = {
    new ValidatingMarketDataReader(reader, RollingAveragePriceValidator, new DayChangePriceValidator(reader))
  }

  def pivot(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): PivotTableDataSource = {
    //    val mds = marketDataSets(marketDataIdentifier.selection)
    //    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    pivotCache.memoize((marketDataIdentifier, marketDataType), {
      val reader = new NormalMarketDataReader(this, marketDataIdentifier)
      val validatingReader = validate(reader)

      new MarketDataPivotTableDataSource(validatingReader, PivotEdits.Null, Some(this), marketDataIdentifier, marketDataType)
    })
  }

  def pivot(selection: MarketDataSelection, marketDataType: MarketDataType): PivotTableDataSource =
    pivot(latestMarketDataIdentifier(selection), marketDataType)

  def versionForMarketDataVersion(marketDataVersion: MarketDataVersion): Int = {
    marketDataVersion match {
      case SpecificMarketDataVersion(v) => v
      case SnapshotMarketDataVersion(ss) => snapshotFromID(ss.id).get.version
    }
  }

  def readLatest[T <: MarketData](marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey): Option[T] = {
    val id = MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key)
    readLatest(id) match {
      case None => None
      case Some(data) => data.data.map(_.asInstanceOf[T])
    }
  }

  def readLatest[T <: MarketData](id: MarketDataID) = {
    db.readLatest(id)
  }

  def latestPricingGroupVersions: Map[PricingGroup, Int] = {
    val lookup = db.latestVersionForMarketDataSets()
    Map() ++ pricingGroupsDefinitions.mapValues {
      marketDataSets => marketDataSets.map(mds => lookup.getOrElse(mds, 0)).max
    }
  }

  def latestExcelVersions: Map[String, Int] = db.latestExcelVersions

  def latestMarketDataIdentifier(selection: MarketDataSelection) = MarketDataIdentifier(selection, latest(selection))

  def latest(selection: MarketDataSelection): Int = {
    val versions = latestPricingGroupVersions.get(selection.pricingGroup).toList :::
      latestExcelVersions.get(selection.excel).toList

    if (versions.isEmpty) 0 else versions.max
  }

  def latestSnapshot(pricingGroup: PricingGroup, observationDay: Day): Option[SnapshotID] = {
    tags.latestSnapshot(pricingGroup, observationDay)
  }

  def saveAll(marketDataSet: MarketDataSet, observationPoint: ObservationPoint, data: Map[MarketDataKey, MarketData]): SaveResult = {
    val dataX = for ((marketDataKey, marketData) <- data) yield {
      MarketDataEntry(observationPoint, marketDataKey, marketData)
    }
    save(Map(marketDataSet -> dataX))
  }

  def save(marketDataSetToData: Map[MarketDataSet, Iterable[MarketDataEntry]]): SaveResult = {
    val setToUpdates = marketDataSetToData.map {
      case (marketDataSet, values) => {
        (marketDataSet, values.map(entry => entry.toUpdate(db.readLatest(entry.dataIdFor(marketDataSet)))))
      }
    }
    saveActions(setToUpdates)
  }

  def saveActions(marketDataSetToData: Map[MarketDataSet, Iterable[MarketDataUpdate]]): SaveResult = this.synchronized {
    val changedMarketDataSets = new scala.collection.mutable.HashMap[MarketDataSet, (Set[Day], Int)]()
    val allChangedDays = new ListBuffer[Day]

    var maxVersion = 0
    for ((marketDataSet, data) <- marketDataSetToData.toList.sortBy(_._1.name)) {
      maxVersion = scala.math.max(maxVersion, saveActions(data, marketDataSet, changedMarketDataSets))
    }

    for ((pricingGroup, marketDataSets) <- pricingGroupsDefinitions) {
      val changesForThisPricingGroup = changedMarketDataSets.filterKeys(marketDataSets)
      if (changesForThisPricingGroup.nonEmpty) {
        val maxVersion = changesForThisPricingGroup.values.maximum(_._2)
        broadcaster.broadcast(PricingGroupMarketDataUpdate(pricingGroup, maxVersion))

        val changedDays = changesForThisPricingGroup.values.map(_._1).foldRight(Set[Day]())(_ ++ _)
        val days = observationDaysByPricingGroupCache(pricingGroup)
        for (day <- changedDays if !days.contains(day)) {
          days += day
          broadcaster.broadcast(PricingGroupObservationDay(pricingGroup, day))
        }

        allChangedDays ++= changedDays
      }
    }

    SaveResult(maxVersion, changedMarketDataSets.nonEmpty, Some(allChangedDays.distinct.toList))
  }


  private def saveActions(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet,
                          changedMarketDataSets: scala.collection.mutable.HashMap[MarketDataSet, (Set[Day], Int)]): Int = {

    val SaveResult(innerMaxVersion, update, _) = db.store(data, marketDataSet)
    if (update) {
      changedMarketDataSets(marketDataSet) = (data.flatMap(_.observationPoint.day.toList).toSet, innerMaxVersion)
    }

    MarketDataSet.fromExcel(marketDataSet).map {
      name =>
        if (!excelDataSetsCache.contains(name)) {
          excelDataSetsCache.append(name)
          excelDataSetsCache = excelDataSetsCache.sortWith(_ < _)
          broadcaster.broadcast(ExcelMarketListUpdate(excelDataSetsCache.toList))
        }
        // TODO [02 Jun 2011] Should this be getOrElseUpdate ? Stacy
        val days = observationDaysByExcelCache.getOrElse(name, MSet[Day]())

        data.flatMap(_.observationPoint.day.toList).filterNot(day => days.contains(day)).foreach(day => {
          days += day
          broadcaster.broadcast(ExcelObservationDay(name, day))
        })

        if (update) {
          broadcaster.broadcast(ExcelMarketDataUpdate(name, innerMaxVersion))
        }
    }

    innerMaxVersion
  }

  def save(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey, marketData: MarketData): Int = {
    save(Map(marketDataSet -> List(MarketDataEntry(timedKey.observationPoint, timedKey.key, marketData)))).maxVersion
  }

  def importData(marketDataSelection: MarketDataSelection, observationDay: Day) = {
    importFor(observationDay, marketDataSets(marketDataSelection): _*)
  }

  val importLock = new Object


  def importFor(observationDay: Day, marketDataSets: MarketDataSet*) = importLock.synchronized {
    log.infoWithTime("saving market data: " + observationDay) {
      val updates: Map[MarketDataSet, scala.List[MarketDataUpdate]] = importer.getUpdates(observationDay, marketDataSets: _*)

      log.infoWithTime("Number of updates: " + updates.mapValues(_.toList.size)) {
        saveActions(updates)
      }
    }
  }

  def latestSnapshotFor(marketDataSelection: MarketDataSelection): Option[SnapshotID] = {
    snapshots().filter(_.marketDataSelection == marketDataSelection).optMaxBy(_.id)
  }

  def snapshot(marketDataSelection: MarketDataSelection, doImport: Boolean, observationDay: Day): Option[SnapshotID] = {
    val previousSnapshot: Option[SnapshotIDLabel] = tags.latestSnapshotFor(marketDataSelection)

    val saveResult = importData(marketDataSelection, observationDay)

    getMaxVersion(marketDataSelection).map {
      version =>
        val (snapshotID, justCreated) = tags.snapshot(version, marketDataSelection, observationDay)

        println("snapshotid: " + snapshotID)

        if (justCreated) {
          broadcaster.broadcast(MarketDataSnapshotSet(marketDataSelection, previousSnapshot, snapshotID.label,
            saveResult.affectedObservationDays))
          broadcaster.broadcast(MarketDataSnapshot(List(snapshotID.id.toString)))
        }
        snapshotID
    }
  }

  private def getMaxVersion(marketDataSelection: MarketDataSelection): Option[Int] = {
    val names = marketDataSets(marketDataSelection).map(_.name)
    if (names.isEmpty) None else db.maxVersionForMarketDataSetNames(names)
  }

  def snapshotFromID(snapshotID: Int): Option[SnapshotID] = tags.snapshotFromID(snapshotID)

  def snapshots(): List[SnapshotID] = tags.snapshots()

  def snapshotsByMarketDataSelection(): Map[MarketDataSelection, List[SnapshotIDLabel]] = tags.snapshotsByMarketDataSelection()

  def observationDaysByPricingGroup(): Map[PricingGroup, Set[Day]] = Map() ++ observationDaysByPricingGroupCache.mapValues(Set() ++ _)

  def latestObservationDayForMarketDataSet(marketDataSet: MarketDataSet) = {
    var days = Set[Day]()
    observationDaysByPricingGroupCache.foreach {
      case (pg, obDays) if pricingGroupsDefinitions(pg) contains marketDataSet => {
        days ++= obDays
      }
      case _ =>
    }
    days.toList.sortWith(_ > _).headOption
  }

  def observationDaysByExcel(): Map[String, Set[Day]] = Map() ++ observationDaysByExcelCache.mapValues(Set() ++ _)

  private def marketDataSets(marketDataIdentifier: MarketDataIdentifier): List[MarketDataSet] = marketDataSets(marketDataIdentifier.selection)

  def marketDataSets(marketDataSelection: MarketDataSelection): List[MarketDataSet] = {
    val pgmds = marketDataSelection.pricingGroup.flatMapL(pgl => pricingGroupsDefinitions(PricingGroup(pgl.name)))
    val excelmds = marketDataSelection.excel.toList.map(MarketDataSet.excel(_))

    excelmds ::: pgmds
  }

  def marketDataTypes(marketDataIdentifier: MarketDataIdentifier): List[MarketDataType] = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val mds = marketDataSets(marketDataIdentifier)
    db.marketDataTypes(version, mds)
  }

  def marketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)] = {
    db.latestMarketData(from, to, marketDataType, marketDataSet)
  }

  def queryForObservationDayAndMarketDataKeys(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): List[TimedMarketDataKey] = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val mds = marketDataSets(marketDataIdentifier)
    db.queryForObservationDayAndMarketDataKeys(version, mds, marketDataType)
  }

  def query(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]] = None, observationTimes: Option[Set[ObservationTimeOfDay]] = None,
            marketDataKeys: Option[Set[MarketDataKey]] = None): List[(TimedMarketDataKey, MarketData)] = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val mds = marketDataSets(marketDataIdentifier)
    db.query(version, mds, marketDataType, observationDays, observationTimes, marketDataKeys)
  }

  private var timings = 0.0

  private def timer[T](block: => T) = {
    val stopwatch = new Stopwatch
    val return_value: T = block
    timings += stopwatch.ms / 1000.0
    //Log.info(tableName + ": cumulative time to date: " + timings)
    return_value
  }
}

case class MarketDataExtendedKey(id: Int, marketDataSet: MarketDataSet, marketDataType: MarketDataType, observationTime: ObservationTimeOfDay, marketDataKey: MarketDataKey)

case class MarketDataValueKey(id: Int, value: Map[PField, Any])

class NewSchemaMdDB(db: DBTrait[RichResultSetRow]) extends MdDB {

  import scala.collection.JavaConversions._
  import java.util.concurrent.{ConcurrentHashMap => jCHMap}

  val extendedKeyById: collection.mutable.ConcurrentMap[Int, MarketDataExtendedKey] = new jCHMap[Int, MarketDataExtendedKey]()
  val valueKeyById: collection.mutable.ConcurrentMap[Int, MarketDataValueKey] = new jCHMap[Int, MarketDataValueKey]()

  def initialise() = {
    // read initial cache state from database
    db.query("SELECT * FROM MarketDataExtendedKey") {
      rs => {
        val id = rs.getInt("id")
        val mdSet = MarketDataSet(rs.getString("marketDataSet"))
        val mdType = MarketDataTypes.fromName(rs.getString("marketDataType"))
        val obsTime: ObservationTimeOfDay = ObservationTimeOfDay.fromName(rs.getString("observationTime"))
        val mdKey = rs.getObject[MarketDataKey]("marketDataKey")
        val ek = MarketDataExtendedKey(id, mdSet, mdType, obsTime, mdKey)
        extendedKeyById.put(ek.id, ek)
      }
    }
    println("Created extended key map with " + extendedKeyById.size + " entries")
    db.query("SELECT * FROM MarketDataValueKey") {
      rs => {
        val id = rs.getInt("id")
        val value = rs.getObject[Map[PField, Any]]("value")
        val vk = MarketDataValueKey(id, value)
        valueKeyById.put(vk.id, vk)
      }
    }
  }

  def checkIntegrity(): Unit = {
    /*
    * We cannot (easily?) know we have "duplicates" in the new schema, as the latest value for an observation day's
    * extended key will be taken to be the one having the greatest commitId.
    */
  }

  def readAll(): Unit = {
    // TODO ?
  }

  def marketDataSetNames(): List[String] = {
    db.queryWithResult("SELECT DISTINCT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds FROM MarketDataExtendedKey ORDER BY mds", Map()) {
      rs => rs.getString("mds")
    }
  }

  def observationDaysFor(marketDataSets: List[MarketDataSet]): List[Option[Day]] = {
    /*
    select distinct observationDay
    from MarketDataValue v, MarketDataExtendedKey k
    where v.extendedKey = k.id and k.marketDataSet in ('LIM', 'LimMetals')
    */
    db.queryWithResult((
      select("DISTINCT marketDataSet COLLATE sql_latin1_general_cp1_cs_as, observationDay")
        from ("MarketDataValue v")
        innerJoin("MarketDataExtendedKey k", "k.id" eql "v.extendedKey")
        where ("marketDataSet" in marketDataSets.map(_.name)))) {
      rs => rs.getDayOption("observationDay")
    }
  }

  def excelObservationDays(): List[(String, Option[Day])] = {
    db.queryWithResult((select("DISTINCT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds, observationDay")
      from "MarketData"
      where ("marketDataSet" like "Excel:%"))) {
      rs => (rs.getString("mds").stripPrefix(MarketDataSet.excelPrefix), rs.getDayOption("observationDay"))
    }
  }

  def latestVersionForMarketDataSets(): Map[MarketDataSet, Int] = {
    Map() ++ db.queryWithResult("""
      SELECT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds, max(commitId) AS maxCommitId
        FROM MarketDataExtendedKey ek
  INNER JOIN MarketDataValue v ON v.extendedKey = ek.id
  INNER JOIN MarketDataCommit c ON c.id = v.commitId
       WHERE marketDataSet NOT LIKE 'Excel:%'
    GROUP BY marketDataSet
    """, Map()) {
      rs => MarketDataSet(rs.getString("mds")) -> rs.getInt("maxCommitId")
    }
  }

  def latestExcelVersions(): Map[String, Int] = {
    //    Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet like 'Excel:%' group by marketDataSet ", Map()) {
    //      rs => rs.getString("marketDataSet").stripPrefix("Excel:") → rs.getInt("m")
    //    }
    Map() ++ db.queryWithResult("""
      SELECT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds, MAX(commitId) cid
        FROM MarketDataExtendedKey emdk
  INNER JOIN MarketDataValue mdv
          ON mdv.extendedKey=emdk.id
  INNER JOIN MarketDataCommit mdc
          ON mdc.id=mdv.commitId
       WHERE marketDataSet like 'Excel:%'
    GROUP BY marketDataSet
    """, Map()) {
      rs => rs.getString("mds").stripPrefix("Excel:") → rs.getInt("cid")
    }
  }

  // no update, no max version
  def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet) = SaveResult(-1, false)

  def maxVersionForMarketDataSetNames(names: List[String]): Option[Int] = {
    db.queryWithOneResult("""
        SELECT MAX(commitId) maxCommitId
          FROM MarketDataCommit c
    INNER JOIN MarketDataValue v
	          ON v.commitId=c.id
    INNER JOIN MarketDataExtendedKey ek
            ON ek.id=v.extendedKey
         WHERE marketDataSet IN (:mds)
    """, Map("mds" → names)) {
      _.getInt("maxCommitId")
    }
  }

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)] = {
    // check that a single value key field exists in this type
    if (marketDataType.valueFields.size > 1) throw new RuntimeException("Market data type has multiple value field keys: " + marketDataType);
    // the key against which to put this types values
    val marketDataTypeValueKey: PField = marketDataType.valueFields.head

    case class MarketDataValue(obsDay: Day, extKeyId: Int, valueKeyId: Int, value: Double, commitId: Int, timestamp: Timestamp)
    case class MarketDataDayKey(obsDay: Day, extKeyId: Int)

    db.queryWithResult("""
    SELECT observationDay, extendedKey, valueKey, value, commitId, timestamp
      FROM MarketDataCommit c
INNER JOIN MarketDataValue v ON c.id=v.commitId
INNER JOIN MarketDataExtendedKey ek ON ek.id=v.extendedKey
     WHERE marketDataSet = :mdsName
       AND observationDay >= :from
       AND observationDay <= :to
       AND marketDataType = :mdtName
    """,
      Map("mdsName" → marketDataSet.name, "from" → from, "to" → to, "mdtName" → marketDataType.name)
    ) {
      rs => {
        // extract row tuples
        MarketDataValue(rs.getDay("observationDay"), rs.getInt("extendedKey"), rs.getInt("valueKey"), rs.getDouble("value"), rs.getInt("commitId"), rs.getTimestamp("timestamp"))
      }
    }.groupBy {
      // group by (observationDay, extendedKeyId) -> row
      mdv => MarketDataDayKey(mdv.obsDay, mdv.extKeyId)
      // create a list from the map's entries, then iterate over them to map each entry to a new list
    }.toList.map {
      case (dayKey, dayValues) => {
        // get the information to create the timed key for this grouping
        val extKey = extendedKeyById.get(dayKey.extKeyId).getOrElse(throw new RuntimeException("Failed to retrieve extended key from id: " + dayKey.extKeyId))
        val obsPoint = ObservationPoint(dayKey.obsDay, extKey.observationTime)
        val timedKey = TimedMarketDataKey(obsPoint, extKey.marketDataKey)
        // a function to return the market data value having the highest commit Id
        def mostRecentMarketDataValue(mdv1: MarketDataValue, mdv2: MarketDataValue): MarketDataValue = {
          if (mdv2.commitId >= mdv1.commitId) mdv2 else mdv1
        }
        // find the value maps for each day value, allowing the most recent commits to overwrite the older ones
        var mostRecentTimestamp = new Timestamp(0)
        var maxCommitId = 0
        // create a mapping from each valueKeyId to a list of its dayValues (the list should normally contain only one)
        val valueMaps = dayValues.groupBy {
          dayValue => dayValue.valueKeyId
          // create a list of these entries, using the single day value with the largest commitId from the list
        }.toList.map {
          case (valueKeyId, dayValues) => {
            val dayValue = dayValues.reduceLeft(mostRecentMarketDataValue)
            val valKey = valueKeyById.get(valueKeyId).getOrElse(throw new RuntimeException("Failed to retrieve value key from id: " + valueKeyId))
            mostRecentTimestamp = mostRecentTimestamp.max(dayValue.timestamp)
            maxCommitId = maxCommitId.max(dayValue.commitId)
            valKey.value + (marketDataTypeValueKey -> PivotQuantity(dayValue.value))
          }
        }
        // create the market data from the values
        val marketData = marketDataType.createValue(valueMaps)
        val versionedMarketData = VersionedMarketData(mostRecentTimestamp, maxCommitId, Option(marketData))
        (timedKey, versionedMarketData)
      }
    }
  }

  def marketDataTypes(version: Int, mds: List[MarketDataSet]): List[MarketDataType] = {
    val commitId = version
    db.queryWithResult(select("DISTINCT ek.marketDataType COLLATE sql_latin1_general_cp1_cs_as AS mdt")
      .from("MarketDataExtendedKey ek")
      .innerJoin("MarketDataValue v", "ek.id" eql "v.extendedKey")
      .innerJoin("MarketDataCommit c", "v.commitId" eql "c.id")
      .where("marketDataSet" in mds.map(_.name))
      .and("c.id" lte commitId)) {
      rs => {
        MarketDataTypes.fromName(rs.getString("mdt"))
      }
    }
  }

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType): List[TimedMarketDataKey] = Nil

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = Nil

  def readLatest(id: MarketDataID): Option[VersionedMarketData] = None
}
