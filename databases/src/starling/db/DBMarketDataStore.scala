package starling.db

import starling.marketdata._
import starling.utils.cache.CacheFactory
import starling.daterange._
import java.lang.String
import starling.utils._
import collection.immutable.{Iterable, Map}
import collection.mutable.{ListBuffer, HashSet => MSet}
import starling.gui.api._
import starling.utils.ImplicitConversions._
import starling.manager.Broadcaster

trait MarketDataSources {
  def marketDataSets: Set[MarketDataSet]
  def marketDataSetsFor[T <: MarketDataSource: Manifest]: Set[MarketDataSet]
  def sourcesFor(marketDataSet: MarketDataSet): List[MarketDataSource]
}

object MarketDataSources {
  object Null extends MarketDataSources {
    def marketDataSets = Set.empty[MarketDataSet]
    def marketDataSetsFor[T <: MarketDataSource : Manifest] = Set.empty[MarketDataSet]
    def sourcesFor(marketDataSet: MarketDataSet) = List.empty[MarketDataSource]
  }
}

class ImmutableMarketDataSources(sources: List[MarketDataSource]) extends MarketDataSources {
  def marketDataSets = sourcesBySet.keySet
  def marketDataSetsFor[T <: MarketDataSource : Manifest] = {
    sourcesBySet.filterValues(l => l.find(_.isInstanceOf[T]).isDefined).keySet
  }
  def sourcesFor(marketDataSet: MarketDataSet) = sourcesBySet(marketDataSet)

  def +(marketDataSource: MarketDataSource) = new ImmutableMarketDataSources(marketDataSource :: sources)

  private val sourcesBySet = sources.toMultiMapWithKeys(_.marketDataSet).withDefaultValue(Nil)
}

class MutableMarketDataSources(initial: List[MarketDataSource]) extends MarketDataSources {
  def marketDataSets = sources.get.marketDataSets
  def marketDataSetsFor[T <: MarketDataSource : Manifest] = sources.get.marketDataSetsFor[T]
  def sourcesFor(marketDataSet: MarketDataSet) = sources.get.sourcesFor(marketDataSet)

  def +=(marketDataSource: MarketDataSource) = { sources.update(_ + marketDataSource); this }

  private val sources = new SynchronizedVar[ImmutableMarketDataSources](new ImmutableMarketDataSources(initial))
}

class DBMarketDataStore(db: MdDB, tags: MarketDataSnapshots, val marketDataSources: MarketDataSources,
  broadcaster: Broadcaster, dataTypes: MarketDataTypes) extends MarketDataStore with Log {

  db.checkIntegrity()

  val importer = new MarketDataImporter(this)
  val pivotCache = CacheFactory.getCache("MarketDataStore.pivotCache")

  def marketDataTypes = dataTypes

  def readAll() {
    snapshotsByMarketDataSelection() //read MarketDataSnapshots table
    db.readAll()
  }

  def sourcesFor(marketDataSet: MarketDataSet) = marketDataSources.sourcesFor(marketDataSet)
  def sourcesFor(pricingGroup: PricingGroup) = marketDataSets(MarketDataSelection(Some(pricingGroup))).flatMap(sourcesFor(_))

  private lazy val excelDataSetsCache = new SynchronizedVar(db.marketDataSetNames().collect {
    case name if name.startsWith(MarketDataSet.excelPrefix) => name.stripPrefix(MarketDataSet.excelPrefix)
  }) {
    override def set(values: List[String]) { super.set(values.sortWith(_ < _)) }
  }

  private lazy val (observationDaysByPricingGroupCache, observationDaysByExcelCache) = {
    val observationDays = db.observationDaysByMarketDataSet

    val observationDaysByPricingGroupCache = pricingGroupsDefinitions.mapValues { marketDataSets =>
      new MSet() ++ marketDataSets.flatMap(marketDataSet => observationDays.getOrElse(marketDataSet.name, Nil))
    }.withDefaultValue(new MSet())

    val observationDaysByExcelCache =
      observationDays.filterKeys(_.startsWith(MarketDataSet.excelPrefix)).mapValues(new MSet() ++ _).withDefaultValue(new MSet())

    (observationDaysByPricingGroupCache, observationDaysByExcelCache)
  }

  def excelDataSets: List[String] = excelDataSetsCache.get

  def pricingGroupForName(name: String) = MarketDataStore.pricingGroupForName(name)

  def versionForSnapshot(snapshotID: SnapshotID) = {
    tags.versionForSnapshot(snapshotID)
  }

  def validate(reader: MarketDataReader): MarketDataReader = {
    new ValidatingMarketDataReader(reader, RollingAveragePriceValidator, new DayChangePriceValidator(reader))
  }

  def versionForMarketDataVersion(marketDataVersion: MarketDataVersion): Int = {
    marketDataVersion match {
      case SpecificMarketDataVersion(v) => v
      case SnapshotMarketDataVersion(ss) => snapshotFromID(ss.id).get.version
    }
  }

  def readLatest(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey): Option[VersionedMarketData] =
    readLatest(MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key, dataTypes))

  def readLatest[T <: MarketData](id: MarketDataID) = db.readLatest(id)

  def latestPricingGroupVersions: Map[PricingGroup, Int] = {
    val lookup = db.latestVersionForMarketDataSets()
    Map() ++ pricingGroupsDefinitions.mapValues {
      marketDataSets => marketDataSets.map(mds => lookup.getOrElse(mds, 0)).max
    }
  }

  def latestExcelVersions: Map[MarketDataSet, Int] = db.latestExcelVersions

  def latestMarketDataIdentifier(selection: MarketDataSelection) = MarketDataIdentifier(selection, latest(selection))

  def latest(selection: MarketDataSelection): Int = {
    val versions = latestPricingGroupVersions.get(selection.pricingGroup).toList :::
      latestExcelVersions.get(selection.excel.map(MarketDataSet.excel(_))).toList

    if (versions.isEmpty) 0 else versions.max
  }

  def latestSnapshot(pricingGroup: PricingGroup) = tags.latestSnapshot(pricingGroup)

  def saveAll(marketDataSet: MarketDataSet, observationPoint: ObservationPoint, data: Map[MarketDataKey, MarketData]): SaveResult = {
    val dataX = data.map { case (marketDataKey, marketData) => MarketDataEntry(observationPoint, marketDataKey, marketData) }

    save(Map(marketDataSet -> dataX.toList))
  }

  def save(marketDataSetToData: MultiMap[MarketDataSet, MarketDataEntry]): SaveResult = {
    val setToUpdates = marketDataSetToData.map { case (marketDataSet, values) => {
      (marketDataSet, values.map(entry => entry.toUpdate(db.readLatest(entry.dataIdFor(marketDataSet, dataTypes)))))
    } }

    update(setToUpdates)
  }

  def update(marketDataSetToData: MultiMap[MarketDataSet, MarketDataUpdate]): SaveResult = {
    if (marketDataSetToData.multiSize == 0) SaveResult.Null else this.synchronized {
      val changedMarketDataSets = new scala.collection.mutable.HashMap[MarketDataSet, (Set[Option[Day]], Int)]()
      val allChangedDays = new ListBuffer[Day]

      val previousLatestPricingGroupVersions = latestPricingGroupVersions
      var maxVersion = 0
      for ((marketDataSet, data) <- marketDataSetToData.toList.sortBy(_._1.name)) {
        maxVersion = scala.math.max(maxVersion, saveActions(data, marketDataSet, changedMarketDataSets))
      }

      for ((pricingGroup, marketDataSets) <- pricingGroupsDefinitions) {
        val changesForThisPricingGroup = changedMarketDataSets.filterKeys(marketDataSets)
        if (changesForThisPricingGroup.nonEmpty) {
          val changedDays = changesForThisPricingGroup.values.map(_._1).foldRight(Set[Option[Day]]())(_ ++ _).toList.somes
          val maxVersion = changesForThisPricingGroup.values.maximum(_._2)
          val previousVersion = previousLatestPricingGroupVersions(pricingGroup)
          broadcaster.broadcast(PricingGroupMarketDataUpdate(pricingGroup, maxVersion, previousVersion, changedDays))

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
  }

  private def saveActions(data: List[MarketDataUpdate], marketDataSet: MarketDataSet,
    changedMarketDataSets: scala.collection.mutable.HashMap[MarketDataSet, (Set[Option[Day]], Int)]): Int = {

    if (data.isEmpty) SaveResult.Null.maxVersion else {
      val SaveResult(innerMaxVersion, update, _) = db.store(data, marketDataSet)

      if (update) {
        changedMarketDataSets(marketDataSet) = (data.map(_.observationPoint.day).toSet, innerMaxVersion)
      }

      MarketDataSet.fromExcel(marketDataSet).map { name =>
        if (!excelDataSetsCache.get.contains(name)) {
          excelDataSetsCache.update(name :: _)
          broadcaster.broadcast(ExcelMarketListUpdate(excelDataSetsCache.get))
        }
        // TODO [02 Jun 2011] Should this be getOrElseUpdate ? Stacy
        val days = observationDaysByExcelCache(name)

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
  }

  def save(marketDataSet: MarketDataSet, timedKey: TimedMarketDataKey, marketData: MarketData): Int = {
    save(Map(marketDataSet -> List(MarketDataEntry(timedKey.observationPoint, timedKey.key, marketData)))).maxVersion
  }


  def importData(marketDataSelection: MarketDataSelection, observationDay: Day) = {
    val marketDataSets = marketDataSelection.pricingGroup.flatMapL(pgl => pricingGroupsDefinitions(PricingGroup(pgl.name)))

    importFor(observationDay, marketDataSets: _*)
  }

  val importLock = new Object

  def importFor(observationDay: Day, marketDataSets: MarketDataSet*) =
    blockSimultaneousDBAccess(observationDay, marketDataSets: _*)
    //blockSimultaneousImports(observationDay, marketDataSets: _*)

  private def blockSimultaneousImports(observationDay: Day, marketDataSets: MarketDataSet*) = importLock.synchronized {
    log.infoWithTime("saving market data: " + observationDay) {
      val updates: MultiMap[MarketDataSet, MarketDataUpdate] = importer.getUpdates(observationDay, marketDataSets: _*)

      log.infoWithTime("Number of updates: " + updates.mapValues(_.toList.size)) {
        update(updates)
      }
    }
  }

  private def blockSimultaneousDBAccess(observationDay: Day, marketDataSets: MarketDataSet*) = {
    log.infoWithTime("saving market data: " + observationDay) {
      val externalData = importer.getExternalData(observationDay, marketDataSets: _*)

      importLock.synchronized {
        val updates = log.infoWithTime("Calculating updates") {
          importer.getUpdates(externalData)
        }

        log.infoWithTime("Applying updates: " + updates.mapKeys(_.name).mapValues(_.toList.size)) {
          update(updates)
        }
      }
    }
  }

  def snapshot(marketDataIdentifier: MarketDataIdentifier, snapshotType:SnapshotType, observationDay : Option[Day]): SnapshotID = {

    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val (snapshotID, justCreated) = tags.snapshot(version, marketDataIdentifier.selection, snapshotType, observationDay)

    if (justCreated) {
      broadcaster.broadcast(MarketDataSnapshot(snapshotID.label, snapshotType == SnapshotType.Valuation, observationDay))
    }
    snapshotID
  }

  def snapshotFromID(snapshotID: Int): Option[SnapshotID] = tags.snapshotFromID(snapshotID)
  def snapshots(): List[SnapshotID] = tags.snapshots()
  def snapshotsByMarketDataSelection(): MultiMap[MarketDataSelection, SnapshotIDLabel] = tags.snapshotsByMarketDataSelection()
  def observationDaysByPricingGroup(): Map[PricingGroup, Set[Day]] = observationDaysByPricingGroupCache.mapValues(_.toSet)
  def observationDaysByExcel(): Map[String, Set[Day]] = observationDaysByExcelCache.mapValues(_.toSet)

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

  def latestObservationDayFor(pricingGroup: PricingGroup, marketDataType: MarketDataTypeName) = {
    db.latestObservationDaysFor(pricingGroupsDefinitions(pricingGroup), marketDataType)
  }

  private def marketDataSets(marketDataIdentifier: MarketDataIdentifier): List[MarketDataSet] = marketDataSets(marketDataIdentifier.selection)

  def marketDataSets = marketDataSources.marketDataSets

  def marketDataSets(marketDataSelection: MarketDataSelection): List[MarketDataSet] = {
    val pgmds = marketDataSelection.pricingGroup.flatMapL(pgl => pricingGroupsDefinitions(PricingGroup(pgl.name)))
    val excelmds = marketDataSelection.excel.toList.map(MarketDataSet.excel(_))

    excelmds ::: pgmds
  }

  def availableMarketDataTypes(marketDataIdentifier: MarketDataIdentifier): List[MarketDataType] = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val mds = marketDataSets(marketDataIdentifier)
    marketDataIdentifier.selection.pricingGroup match {
      case Some(PricingGroup.Metals) => (new CountryBenchmarkDataType() :: db.marketDataTypes(version, mds).toList).distinct
      case _ => db.marketDataTypes(version, mds).toList
    }

  }

  def marketData(from: Day, to: Day, marketDataType: MarketDataTypeName, marketDataSet: MarketDataSet) = {
    db.latestMarketData(from, to, marketDataType, marketDataSet)
  }

  def queryForObservationDayAndMarketDataKeys(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataTypeName): List[TimedMarketDataKey] = {
    val version = versionForMarketDataVersion(marketDataIdentifier.marketDataVersion)
    val mds = marketDataSets(marketDataIdentifier)
    db.queryForObservationDayAndMarketDataKeys(version, mds, marketDataType).toList
  }

  def query(marketDataIdentifier: MarketDataIdentifier, marketDataType: MarketDataTypeName,
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
