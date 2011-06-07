package starling.db

import starling.utils.sql.QueryBuilder._
import collection.immutable.TreeMap
import starling.quantity.Percentage
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.richdb.RichResultSetRow
import starling.utils.sql._
import starling.gui.api._
import starling.pivot.PivotTableDataSource
import starling.pivot.{Field => PField}
import starling.utils.cache.CacheFactory
import starling.daterange._
import collection.mutable.{ListBuffer, HashSet => MSet}
import starling.calendar.Clock

import starling.utils.Pattern._
import starling.utils._
import org.springframework.dao.DuplicateKeyException
import java.lang.{RuntimeException, String}
import java.util.concurrent.atomic.AtomicInteger


// TODO [07 Sep 2010] move me somewhere proper
case class RelativeImpliedVolData(vols: Map[DateRange,Map[Double,Percentage]]) {
  def absolute(prices : PriceData) : ImpliedVolData = {
    val data = vols.flatMap {
      case (period, row) => {
        // lame hack - why, trinity? WHY???
        val exerciseDay = period.firstDay - 2
        val strike: Double = prices.prices(exerciseDay).value.value
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

trait MarketDataSource { self =>
  def read(day:Day): Map[(Day, Day, MarketDataType), List[MarketDataEntry]]

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

case class MarketDataSet(name: String) extends Named

object MarketDataSet extends StarlingEnum(classOf[MarketDataSet]) {
  val excelPrefix = "Excel:"
  def excel(name:String) = {
    if (name == "Official:Metals") {
      MarketDataSet.ManualMetals
    } else {
      MarketDataSet(excelPrefix + name)
    }
  }
  def fromExcel(marketDataSet:MarketDataSet) = {
    if (marketDataSet.name.startsWith(excelPrefix)) {
      Some( marketDataSet.name.substring(excelPrefix.length) )
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
  val TrinityLive = MarketDataSet("Trinity/Live")
  val GalenaLive = MarketDataSet("Galena/Live")
  val GalenaFullCurve = MarketDataSet("Galena/FullCurve")
  val VarMetalsFreight = MarketDataSet("Var:Metals+Freight")
  val VarGalenaLondon = MarketDataSet("Var:GalenaLondon")
  val VarGalenaOil= MarketDataSet("Var:GalenaOil")
  val LimMetals = MarketDataSet("LimMetals") //Refined Metals
  val ManualMetals = MarketDataSet("ManualMetals")
  val Starling = MarketDataSet("Starling")
  val TrinityDiscountFactorCSV = MarketDataSet("TrinityDiscountFactorCSV")
}

object MarketDataStore {
  import MarketDataSet._

  val pricingGroupsDefinitions = Map[PricingGroup,List[MarketDataSet]](
    PricingGroup.Metals -> List(ManualMetals, LimMetals, TrinityDiscountFactorCSV),
    PricingGroup.LimOnly -> List(LIM),
    PricingGroup.System -> List(Starling, LIM, System),
    PricingGroup.Crude-> List(Starling, LIM, Crude),
    PricingGroup.LondonDerivatives -> List(Starling, LondonDerivatives, LIM),
    PricingGroup.GasolineRoW -> List(Starling, GasolineRoW, LIM),
    PricingGroup.BarryEckstein -> List(Starling, BarryEckstein, System, LIM),
    PricingGroup.LondonDerivativesOptions -> List(Starling, LondonDerivativesOptions, System, LIM)
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
    pricingGroupsDefinitions.get(pricingGroup).flatMap{ sets => (manuallyEditableMarketDataSets & sets.toSet).headOption }
  }
}

/**
 * Wraps a data type to provide version information.
 */

trait MarketDataStore {
  val pricingGroupsDefinitions = MarketDataStore.pricingGroupsDefinitions
  val pricingGroups = MarketDataStore.pricingGroups

  def applyOverrideRule(marketDataType: MarketDataType, allDataForKeyAndDay: List[Map[PField, Any]]): List[Map[PField, Any]]

  def excelDataSets: List[String]

  def importData(marketDataSelection:MarketDataSelection, observationDay : Day): (Int, Boolean)
  def importFor(observationDay: Day, marketDataSets: MarketDataSet*): (Int, Boolean)

  def latest(selection:MarketDataSelection): Int
  def latestExcelVersions: Map[String, Int]
  def latestMarketDataIdentifier(selection: MarketDataSelection): MarketDataIdentifier
  def latestObservationDayForMarketDataSet(marketDataSet: MarketDataSet): Option[Day]
  def latestPricingGroupVersions: Map[PricingGroup, Int]
  def latestSnapshot(pricingGroup: PricingGroup, observationDay: Day): Option[SnapshotID]

  def marketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)]
  def marketDataTypes(marketDataIdentifier: MarketDataIdentifier): List[MarketDataType]

  def observationDays(pricingGroup: PricingGroup, from: Day, to: Day): List[Day]
  def observationDays(marketDataIdentifier: MarketDataIdentifier, from: Day, to: Day): List[Day]
  def observationDaysByExcel(): Map[String, Set[Day]]
  def observationDaysByPricingGroup(): Map[PricingGroup, Set[Day]]

  def pivot(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): PivotTableDataSource
  def pivot(selection: MarketDataSelection, marketDataType: MarketDataType): PivotTableDataSource

  def query(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]] = None, observationTimes: Option[Set[ObservationTimeOfDay]] = None,
            marketDataKeys: Option[Set[MarketDataKey]] = None): List[(TimedMarketDataKey, MarketData)]

  def queryForObservationDayAndMarketDataKeys(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): List[TimedMarketDataKey]

  def readLatest[T <: MarketData](marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey): Option[T]

  def save(marketDataSetToData: Map[MarketDataSet, Iterable[MarketDataEntry]]): (Int, Boolean)
  def save(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey, marketData: MarketData): Int
  def saveAll(marketDataSet: MarketDataSet, observationPoint: ObservationPoint, data: Map[MarketDataKey,MarketData]): (Int, Boolean)

  def snapshot(marketDataSelection: MarketDataSelection, doImport: Boolean, observationDay: Day) : SnapshotID
  def snapshotsByMarketDataSelection(): Map[MarketDataSelection, List[SnapshotIDLabel]]
  def snapshotFromID(snapshotID: Int): Option[SnapshotID]

  def sourceFor(marketDataSet: MarketDataSet): Option[MarketDataSource]
  def sourcesFor(pricingGroup: PricingGroup): List[MarketDataSource]
}

case class VersionedMarketData(timestamp: Timestamp, version: Int, data: Option[MarketData])

object VersionedMarketData {
  def apply(key: MarketDataKey, rs: ResultSetRow) = new VersionedMarketData(rs.getTimestamp("timestamp"), rs.getInt("version"),
    rs.getObjectOption[Any]("data").map(key.unmarshallDB(_)))

  val Delete = Extractor.when[VersionedMarketData](_.data.isEmpty)
  val Save   = Extractor.when[VersionedMarketData](_.data.isDefined)
}

case class MarketDataEntry(observationPoint: ObservationPoint, key: MarketDataKey, data: MarketData) {
  val dataType = key.dataType
  def isEmpty = key.castRows(data).isEmpty
  def toSave(existingData: Option[VersionedMarketData]) = if (isEmpty) None else Some(MarketDataUpdate(timedKey, Some(data), existingData))
  def toUpdate(existingData: Option[VersionedMarketData]) = toSave(existingData).getOrElse(MarketDataUpdate(timedKey, None, existingData))
  def timedKey = TimedMarketDataKey(observationPoint, key)
  def dataIdFor(marketDataSet: MarketDataSet) = MarketDataID(timedKey, marketDataSet)
}

case class MarketDataUpdate(timedKey: TimedMarketDataKey, data: Option[MarketData], existingData: Option[VersionedMarketData]) {
  def observationPoint = timedKey.observationPoint
  def marketDataKey = timedKey.key
  def dataIdFor(marketDataSet: MarketDataSet) = MarketDataID(timedKey, marketDataSet)
  def indexedData(marketDataSet: MarketDataSet) = dataIdFor(marketDataSet) → data
}

// TODO [12 May 2011] move me somewhere proper
class DBMarketDataStore(db: DBTrait[RichResultSetRow], val marketDataSources: Map[MarketDataSet, MarketDataSource],
                        broadcaster: Broadcaster = Broadcaster.Null) extends MarketDataStore {

  locally {
    val q = (select("observationDay, observationTime, marketDataSet, marketDataType, marketDataKey")
              from "MarketData"
              where ("childVersion" isNull)
              groupBy("observationDay, observationTime, marketDataSet, marketDataType, marketDataKey")
              having ("COUNT(marketDataKey)" gt 1))

    val duplicates = db.queryWithResult(q){
      rs => {
        println("!!! :" + rs)
        rs.getString("marketDataSet")
      }
    }

    assert(duplicates.isEmpty, "The MDS is corrupt\n" + q)
  }

  private val currentVersion = new AtomicInteger(
    db.queryWithOneResult[Int]("SELECT MAX(version) as version from MarketData")(_.getInt("version")).getOrElse(1))

  val importer = new MarketDataImporter(this)
  val pivotCache = CacheFactory.getCache("MarketDataStore.pivotCache")

  def readAll() {
    Log.infoWithTime("Reading all market data") {

      snapshotsByMarketDataSelection() //read MarketDataTag table

      val errors = scala.collection.mutable.HashSet[String]()
      var month:Option[Month] = None
      db.query( (select ("*") from "MarketData") orderBy Desc("observationDay")) {
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
            case e:Exception => {
              if (!errors.contains(e.getMessage)) {
                e.printStackTrace
                errors += (e.getMessage)
              }
            }
          }
        }
      }
      println(errors.size + " errors")
      errors.foreach{ e => println("  " + e)}
    }
  }

  def sourceFor(marketDataSet:MarketDataSet) = marketDataSources.get(marketDataSet)
  def sourcesFor(pricingGroup: PricingGroup) = marketDataSets(MarketDataSelection(Some(pricingGroup))).flatMap(sourceFor(_))

  private var excelDataSetsCache = {
    scala.collection.mutable.ArrayBuffer[String]() ++ db.queryWithResult("select distinct marketDataSet from MarketData order by marketDataSet", Map()) {
      rs => rs.getString("marketDataSet")
    }.flatMap { name => {
      if (name.startsWith(MarketDataSet.excelPrefix)) {
        Some(name.substring(MarketDataSet.excelPrefix.length))
      } else {
        None
      }
    } }
  }

  private val observationDaysByPricingGroupCache = {
    new scala.collection.mutable.HashMap() ++ pricingGroupsDefinitions.mapValues {
      (marketDataSets) => {
        val list = db.queryWithResult( (select ("distinct observationDay") from "MarketData" where ("marketDataSet" in marketDataSets.map(_.name))) ) {
          rs => rs.getDayOption("observationDay")
        }
        new scala.collection.mutable.HashSet() ++ list.somes
      }
    }
  }

  private val observationDaysByExcelCache = {
    val list: scala.List[(String, Option[Day])] = db.queryWithResult( (select ("distinct marketDataSet, observationDay") from "MarketData" where ("marketDataSet" like "Excel:%")) ) {
      rs => (rs.getString("marketDataSet").substring(MarketDataSet.excelPrefix.length), rs.getDayOption("observationDay"))
    }
    new scala.collection.mutable.HashMap() ++ list.groupBy(_._1).mapValues(new MSet() ++ _.flatMap(_._2.toList))
  }

  def excelDataSets: List[String] = excelDataSetsCache.toList

  def pricingGroupForName(name:String) = MarketDataStore.pricingGroupForName(name)

  def versionForSnapshot(snapshotID:SnapshotID) = {
    db.queryWithOneResult("select version from MarketDataTag where snapshotid = :id", Map("id"->snapshotID.id)) { rs => rs.getInt("version")}.get
  }

  def validate(reader: MarketDataReader): MarketDataReader = {
    new ValidatingMarketDataReader(reader, RollingAveragePriceValidator, new DayChangePriceValidator(reader))
  }

  def pivot(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): PivotTableDataSource = {
//    val mds = marketDataSets(marketDataIdentifier.selection)
//    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    pivotCache.memoize( (marketDataIdentifier, marketDataType), {
      val reader = new NormalMarketDataReader(this, marketDataIdentifier)
      val validatingReader = validate(reader)

      new MarketDataPivotTableDataSource(validatingReader, Some(this), marketDataIdentifier, marketDataType)
    })
  }

  def pivot(selection: MarketDataSelection, marketDataType: MarketDataType): PivotTableDataSource =
    pivot(latestMarketDataIdentifier(selection), marketDataType)

  def versionForMarketDataVersion(marketDataVersion:MarketDataVersion):Int = {
    marketDataVersion match {
      case SpecificMarketDataVersion(v) => v
      case SnapshotMarketDataVersion(ss) => snapshotFromID(ss.id).get.version
    }
  }

  def readLatest[T <: MarketData](marketDataSet:MarketDataSet, timedKey: TimedMarketDataKey) : Option[T] = {
    val id = MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key)
    readFoo(id, None, timedKey.unmarshallDB(_)).asInstanceOf[Option[T]]
  }

  def applyOverrideRule(marketDataType:MarketDataType, allDataForKeyAndDay:List[Map[PField,Any]]): List[Map[PField, Any]] = {
    val dataAsMaps:Map[Map[PField,Any],Map[PField,Any]] =
      Map() ++ allDataForKeyAndDay.map {
        dataAsMap => {
          val keys = marketDataType.keyFields.map(f=> f->dataAsMap(f)).toMap
          val values = dataAsMap.filterKeys(f => !marketDataType.keyFields.contains(f))
          keys -> values
        }
      }
    val m = scala.collection.mutable.HashMap[Map[PField,Any],Map[PField,Any]]()
    dataAsMaps.foreach { case(k,v) => {
      m(k) = v
    }}
    m.map{ case(k,v)=> k++v }.toList
  }


  def latestPricingGroupVersions: Map[PricingGroup, Int] = {
    val lookup = Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet not like 'Excel:%' group by marketDataSet ", Map()) {
      rs=> MarketDataSet(rs.getString("marketDataSet")) -> rs.getInt("m")
    }
    Map() ++ pricingGroupsDefinitions.mapValues {
      marketDataSets => marketDataSets.map(mds => lookup.getOrElse(mds, 0)).max
    }
  }

  def latestExcelVersions: Map[String, Int] = {
    Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet like 'Excel:%' group by marketDataSet ", Map()) {
      rs=> rs.getString("marketDataSet").substring("Excel:".length) -> rs.getInt("m")
    }
  }

  def latestMarketDataIdentifier(selection:MarketDataSelection) = {
    MarketDataIdentifier(selection, SpecificMarketDataVersion(latest(selection)))
  }

  def latest(selection:MarketDataSelection): Int = {
    val versions = selection.pricingGroup.toList.map { pg =>
      latestPricingGroupVersions(pg)
    } ::: selection.excel.toList.map { excel =>
      latestExcelVersions(excel)
    }
    if (versions.isEmpty) 0 else versions.max
  }

  def latestSnapshot(pricingGroup:PricingGroup, observationDay : Day) : Option[SnapshotID] = {
    db.queryWithOneResult("""
    select *
    from MarketDataTag
    where
      pricingGroup = :pricingGroup
      and observationDay = :observationDay
    order by snapshotTime desc
    """, Map("pricingGroup" -> StarlingXStream.write(pricingGroup), "observationDay" -> observationDay)) {
      rs => snapshotIDFromResultSetRow(rs)
    }
  }

  def saveAll(marketDataSet : MarketDataSet, observationPoint : ObservationPoint, data : Map[MarketDataKey,MarketData]): (Int, Boolean) = {
    val dataX = for ((marketDataKey, marketData) <- data) yield {
      MarketDataEntry(observationPoint, marketDataKey, marketData)
    }
    save(Map(marketDataSet -> dataX))
  }

  def save(marketDataSetToData : Map[MarketDataSet, Iterable[MarketDataEntry]]) : (Int, Boolean) = {
    val setToUpdates = marketDataSetToData.map { case (marketDataSet, values) => {
      (marketDataSet, values.map(entry => entry.toUpdate(apply(entry.dataIdFor(marketDataSet)))))
    }}

    saveActions(setToUpdates)
  }

  def saveActions(marketDataSetToData : Map[MarketDataSet, Iterable[MarketDataUpdate]]) : (Int, Boolean) = this.synchronized {
    val changedMarketDataSets = new scala.collection.mutable.HashMap[MarketDataSet, (Set[Day], Int)]()
    var maxVersion = 0
    for ((marketDataSet, data) <- marketDataSetToData.toList.sortBy(_._1.name)) {
      maxVersion = scala.math.max(maxVersion, saveActions(data, marketDataSet, changedMarketDataSets))
    }

    for ((pricingGroup, marketDataSets) <- pricingGroupsDefinitions) {
      val changesForThisPricingGroup = changedMarketDataSets.filterKeys( mds => marketDataSets.contains(mds) )
      if (changesForThisPricingGroup.nonEmpty) {
        val maxVersion = changesForThisPricingGroup.values.maximum(_._2)
        broadcaster.broadcast(PricingGroupMarketDataUpdate(pricingGroup, maxVersion))

        val changedDays = changesForThisPricingGroup.values.map(_._1).foldRight(Set[Day]())(_ ++ _)
        val days = observationDaysByPricingGroupCache(pricingGroup)
        for (day <- changedDays if !days.contains(day)) {
          days += day
          broadcaster.broadcast(PricingGroupObservationDay(pricingGroup, day))
        }
      }
    }
    (maxVersion, changedMarketDataSets.nonEmpty)
  }


  private def saveActions(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet,
                          changedMarketDataSets: scala.collection.mutable.HashMap[MarketDataSet, (Set[Day], Int)]): Int = {
    var update = false
    var innerMaxVersion = 0

    db.inTransaction( dbWriter =>
      data.map { action =>
        updateIt(dbWriter, action.dataIdFor(marketDataSet), action.existingData, action.data).foreach { result =>
          if (result._1) update = true
          innerMaxVersion = scala.math.max(innerMaxVersion, result._2)
        }
      }
    )

    if (update) {
      changedMarketDataSets(marketDataSet) = (data.flatMap(_.observationPoint.day.toList).toSet, innerMaxVersion)
    }

    MarketDataSet.fromExcel(marketDataSet).map { name =>
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

  def save(marketDataSet:MarketDataSet, timedKey: TimedMarketDataKey, marketData:MarketData):Int = {
    save(Map(marketDataSet -> List( MarketDataEntry(timedKey.observationPoint, timedKey.key, marketData) ) ))._1
  }

  def importData(marketDataSelection:MarketDataSelection, observationDay : Day) = {
    importFor(observationDay, marketDataSets(marketDataSelection) : _*)
  }

  val importLock = new Object


  def importFor(observationDay: Day, marketDataSets: MarketDataSet*) = importLock.synchronized {
    Log.infoWithTime("saving market data: " + observationDay) {
      val updates: Map[MarketDataSet, scala.List[MarketDataUpdate]] = importer.getUpdates(observationDay, marketDataSets: _*)
      Log.debug("Number of updates: " + updates.mapValues(_.toList.size))

      saveActions(updates)
    }
  }

  def snapshot(marketDataSelection:MarketDataSelection, doImport:Boolean, observationDay : Day) : SnapshotID = {

    importData(marketDataSelection, observationDay)

    val version = db.queryWithOneResult(
      "select max(version) m from MarketData where marketDataSet in (:mds)",
      Map("mds"->marketDataSets(marketDataSelection).map(_.name))) {
      rs => rs.getInt("m")
    }.get

    import QueryBuilder._
    val snapshotID = db.queryWithOneResult(
      (select ("*") from "MarketDataTag" where (("version" eql version) and ("marketDataSelection" eql LiteralString(StarlingXStream.write(marketDataSelection))) and ("observationDay" eql observationDay)))) {
       rs => snapshotIDFromResultSetRow(rs)
    } match {
      case Some(ss) => ss
      case None => {
        val timestamp = new Timestamp()
        val params = Map("snapshotTime"->timestamp, "version" -> version, "marketDataSelection"->StarlingXStream.write(marketDataSelection), "observationDay"->observationDay)
        var id:Option[Long] = None
        db.inTransaction( writer => id = Some(writer.insertAndReturnKey("MarketDataTag", "snapshotid", params, Some(List("snapshotTime", "version", "marketDataSelection", "observationDay")))) )
        broadcaster.broadcast(MarketDataSnapshot(snapshotsByMarketDataSelection))
        SnapshotID(observationDay, id.get.toInt, timestamp, marketDataSelection, version)
      }
    }

    println("snapshotid: " + snapshotID)

    snapshotID
  }

  def latestSnapshotID = {
    db.queryWithOneResult("select max(snapshotid) m from MarketDataTag", Map()) { rs => rs.getInt("m")}.get
  }

  def snapshotFromID(snapshotID : Int): Option[SnapshotID] = {
    db.queryWithOneResult("""
    select *
    from MarketDataTag
    where
      snapshotID = :snapshotID
    """, Map("snapshotID" -> snapshotID))(snapshotIDFromResultSetRow)
  }

  def snapshotsByMarketDataSelection(): Map[MarketDataSelection, List[SnapshotIDLabel]] = {
    db.queryWithResult("select * from MarketDataTag order by snapshotID desc", Map()) {
      rs => snapshotIDFromResultSetRow(rs)
    }.groupBy(_.marketDataSelection).map{ case(selection, snapshots) => selection -> snapshots.map(_.label).sortWith(_ > _) }
  }

  def observationDaysByPricingGroup():Map[PricingGroup,Set[Day]] = Map() ++ observationDaysByPricingGroupCache.mapValues(Set() ++ _)

  def latestObservationDayForMarketDataSet(marketDataSet: MarketDataSet) = {
    var days = Set[Day]()
    observationDaysByPricingGroupCache.foreach{
      case (pg, obDays) if pricingGroupsDefinitions(pg) contains marketDataSet => {
        days ++= obDays
      }
      case _ =>
    }
    days.toList.sortWith(_ > _).headOption
  }

  def observationDaysByExcel():Map[String,Set[Day]] = Map() ++ observationDaysByExcelCache.mapValues(Set() ++ _)

  def snapshots(pricingGroup : PricingGroup):List[SnapshotID] = {
    val snapshotsQuery = (select ("*") from "MarketDataTag" where ("pricingGroup" eql LiteralString(StarlingXStream.write(pricingGroup))) orderBy Desc("snapshotID"))
    db.queryWithResult(snapshotsQuery)(snapshotIDFromResultSetRow)
  }

  private def marketDataSets(marketDataIdentifier:MarketDataIdentifier):List[MarketDataSet] = marketDataSets(marketDataIdentifier.selection)

  def marketDataSets(marketDataSelection:MarketDataSelection):List[MarketDataSet] = {
    val pgmds:List[MarketDataSet] = marketDataSelection.pricingGroup.toList.flatMap {
      pgl=> pricingGroupsDefinitions(PricingGroup(pgl.name))
    }
    val excelmds:List[MarketDataSet] = marketDataSelection.excel.toList.map(excel=>MarketDataSet.excel(excel))
    excelmds ::: pgmds
  }

  def marketDataTypes(marketDataIdentifier:MarketDataIdentifier) : List[MarketDataType] = {
    queryForMarketDataIdentifier(
      marketDataIdentifier,
      "distinct marketDataType",
      None,
      rs => rs.getObject[MarketDataType]("marketDataType")
    )
  }

  def marketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet): List[(TimedMarketDataKey, VersionedMarketData)] = {
    val query = (
      select ("observationDay, observationTime, marketDataKey, data, timestamp, version")
      from "MarketData"
      where (("marketDataSet") eql marketDataSet.name)
        and ("observationDay" gte from)
        and ("observationDay" lte to)
        and ("marketDataType" eql AnObject(marketDataType))
        and ("childVersion" isNull)
    )
    val results = db.queryWithResult(query) { rs => {
      val key: MarketDataKey = rs.getObject[MarketDataKey]("marketDataKey")
      (TimedMarketDataKey(observationPoint(rs), key), VersionedMarketData(key, rs))
    }}
    val check = results.groupBy(e => (e._1, e._2))
    check.foreach {
      case (k, v) if v.size > 1 => {
        Log.error("MDS has gotten corrupt: " + (marketDataSet, marketDataType, k, v))
        throw new Exception("Duplicate data in MDS for " + (marketDataSet, marketDataType, k, v))
      }
      case _ =>
    }
    results
  }

  private def observationPoint(rs: ResultSetRow) =
    ObservationPoint(rs.getDay("observationDay"), ObservationTimeOfDay.fromName(rs.getString("observationTime")))

  private def queryForMarketDataIdentifier[T](
        marketDataIdentifier:MarketDataIdentifier,
        selectQuery : String,
        clause:Option[Clause],
        f:RichResultSetRow=>T) = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val versionClause = (("version" lte version) and ( (("childVersion") isNull) or ("childVersion" gt version)) )
    val query = (
      select (selectQuery)
      from "MarketData"
      where ((("marketDataSet" in marketDataSets(marketDataIdentifier).map(_.name)) and versionClause) andMaybe clause)
    )
    db.queryWithResult(query) { rs => f(rs) }
  }

  def queryForObservationDayAndMarketDataKeys(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType): List[TimedMarketDataKey] = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)

    db.queryWithResult(
      (select ("distinct observationTime, observationDay, marketDataKey")
       from ("MarketData")
       where typeAndSnapshotClauses(marketDataIdentifier, marketDataType)) ) {
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

  def query(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]] = None, observationTimes: Option[Set[ObservationTimeOfDay]] = None,
            marketDataKeys: Option[Set[MarketDataKey]] = None): List[(TimedMarketDataKey, MarketData)] = {

    val observationDayClause = {
      observationDays.map { days => {
        val actualDays = days.toList.somes
        val inClause = In(Field("observationDay"), actualDays)
        if (days.contains(None)) {
          (inClause or ("observationDay" isNull))
        } else {
          inClause
        }
      } }
    }
    val observationTimeClause = observationTimes.map(times => In(Field("observationTime"), times.map(_.name)))
    val keyClause = marketDataKeys.map(keys => In(Field("marketDataKey"), keys.map(StarlingXStream.write(_))))
    val mds = marketDataSets(marketDataIdentifier)

    val query = (
      select ("*")
        from "MarketData"
       where typeAndSnapshotClauses(marketDataIdentifier, marketDataType) :: observationDayClause.toList :::
             observationTimeClause.toList ::: keyClause.toList
       orderBy "observationDay, marketDataKey, marketDataSet".desc
    )

    val data = new ListBuffer[(TimedMarketDataKey, MarketData)]()
    var latestKey:Option[(String,Option[Day],MarketDataKey)] = None
    val latestValues = new scala.collection.mutable.HashMap[String,(MarketData)]()

    def addEntry(timeOfDay:String, day:Option[Day], key:MarketDataKey) {
      val allDataForKeyAndDay: List[Map[PField, Any]] = mds.reverse.flatMap(mds => {
        latestValues.get(mds.name).toList.flatMap(data => key.castRows(data))
      })
      val rowsFromOneMap = applyOverrideRule(marketDataType, allDataForKeyAndDay)
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

        optionMarketData.map { marketData =>
          latestKey match {
            case None => {
              latestKey = Some(key)
              latestValues.clear()
              latestValues(marketDataSet) = marketData
            }
            case Some(`key`) => {
              latestValues(marketDataSet) = marketData
            }
            case Some((timeOfDay,day,k)) => {
              addEntry(timeOfDay,day,k)
              latestKey = Some(key)
              latestValues.clear()
              latestValues(marketDataSet) = marketData
            }
          }
        }
      }
    }

    latestKey match {
      case Some((time,day,k)) => addEntry(time,day,k)
      case None =>
    }

    data.toList
  }

  private def typeAndSnapshotClauses(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataType) = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)

    (("marketDataSet" in marketDataSets(marketDataIdentifier).map(_.name))
       and (("version" lte version) and ( (("childVersion") isNull) or ("childVersion" gt version)) )
       and ("marketDataType" eql LiteralString(StarlingXStream.write(marketDataType))))
  }

  def observationDays(pricingGroup:PricingGroup, from : Day, to :Day) : List[Day] = {
    observationDays(MarketDataIdentifier(MarketDataSelection(Some(pricingGroup)), SpecificMarketDataVersion(latestPricingGroupVersions(pricingGroup))), from, to)
  }

  def observationDays(marketDataIdentifier:MarketDataIdentifier, from : Day, to :Day) : List[Day] = {
    queryForMarketDataIdentifier(
      marketDataIdentifier,
      "distinct observationDay",
      Some( (("observationDay" gte from) and ("observationDay" lte to)) ),
      (rs) => rs.getDay("observationDay")
    )
  }

  private def snapshotIDFromResultSetRow(rs : RichResultSetRow) : SnapshotID = {
    SnapshotID(rs.getDay("observationDay"), rs.getInt("snapshotID"), rs.getTimestamp("snapshotTime"), rs.getObject[MarketDataSelection]("marketDataSelection"), rs.getInt("version"))
  }

  // VersionedDatabase

  // returns Query DSL object to find the row identified by the key at the given version,
  // where Some(version) indicates a specific version and None indicates the most recent
  // version.
  def queryForVersion(key : MarketDataID, version : Option[Int]) : Query = {
    import QueryBuilder._

    (select ("timestamp, version, data")
       from ("MarketData")
       where (conditions(key)
         and version.map("version" eql _).getOrElse("childVersion" isNull)))
  }

  // gets the specified version of the curve. If the version is None then the latest
  // available version is fetched.
  def apply(id: MarketDataID, version : Option[Int] = None) : Option[VersionedMarketData] = {
    db.queryWithOneResult(queryForVersion(id, version)) { VersionedMarketData(id.subTypeKey, _) }
  }

  def currentVersions(ids: Iterable[MarketDataID]) = {
    val query = (
      select ("timestamp, version, data, marketDataKey")
      from ("MarketData")
      where ("marketDataKey" in (ids.map(_.subTypeKey)))
      and ("childVersion" isNull)
    )

    val keyToVersionedData: Map[MarketDataKey, VersionedMarketData] = db.queryWithResult(query) { rs => {
      val id = rs.getObject[MarketDataKey]("marketDataKey")

      id → VersionedMarketData(id, rs)
    }}.toMap

    ids.toMapWithValues(id => keyToVersionedData.get(id.subTypeKey))
  }

  protected def readFoo(key : MarketDataID, version : Option[Int], unmarshallar: Any => MarketData) = {
    import QueryBuilder._
    val versionClause = version match {
      case Some(v) => (("version" lte v) and ((("childVersion") gt v) or ("childVersion" isNull)))
      case None => ("childVersion" isNull)
    }
    val query =
      (select ("data")
         from ("MarketData")
        where (conditions(key) and versionClause)
      )

    db.queryWithOneResult(query)(rs => rs.getObjectOption[Any]("data")).flatOpt.map(unmarshallar)
  }

  def updateIt(dbWriter: DBWriter, id: MarketDataID, existingData: Option[VersionedMarketData], maybeData: Option[MarketData]) = {
    val timestamp = Clock.timestamp

    (existingData, maybeData) match {
      case (None, None) => None
      case (Some(VersionedMarketData(timestamp, v, Some(d))), Some(data)) if d == data => Some( (false, v) )
      case (_, x) => {
        val valueForDataColumn = x.map(v => StarlingXStream.write(v.marshall)).getOrElse(null) // null means delete
        import QueryBuilder._

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
            val values = id.conditions + ("timestamp" →  timestamp) + ("data" →  valueForDataColumn) + ("version" → nextVersion)

            dbWriter.insert("MarketData", values)
          }

          result = Some(nextVersion.toInt)
        //}
        Some((true, result.get))
      }
    }
  }

  private var timings = 0.0

  private def timer[T](block : => T) = {
    val stopwatch = new Stopwatch
    val return_value : T = block
    timings += stopwatch.ms / 1000.0
    //Log.info(tableName + ": cumulative time to date: " + timings)
    return_value
  }

  private def conditions(key : MarketDataID) : Clause = conditionsFromMap(key.conditions)

  def conditionsFromMap(conditions:Map[String,Any]) = {
    import QueryBuilder._

    conditions.map {
      case (k, null) => k isNull 
      case (k, v) => k eql literal(v)
    }.reduceLeft((a:Clause, b:Clause) => a and b)
  }

  /**
   * Strings need to be wrapped in a "literalstring" object otherwise strings with dots
   * in are interpreted as column.field references. This is not what would be being stored
   * in a database key, so we escape it all here.
   */
  private def literal(v : Any) : Any = v match {
    case s : String => LiteralString(s)
    case _ => v
  }
}
