package starling.db

import starling.dbx._
import starling.dbx.QueryBuilder._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange._
import java.lang.String
import starling.utils._
import sql._
import starling.calendar.Clock
import scala.collection.JavaConversions._
import starling.quantity.{Quantity, UOM, Percentage}
import scalaz.Scalaz._
import starling.auth.User
import starling.richdb.RichResultSetRow
import collection.mutable.{ListBuffer, Map => MMap}
import concurrent.SyncVar
import starling.pivot.{Row, Field => PField}
import collection.immutable._


class NewSchemaMdDB(db: DBTrait[RichResultSetRow], referenceDataLookup: ReferenceDataLookup) extends MdDB with Log {
  val extendedKeys = JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, MarketDataExtendedKey](
    db.queryWithResult("SELECT * FROM MarketDataExtendedKey") { rs => MarketDataExtendedKey(rs) }.toMapWithKeys(_.id)))

  val valueKeys = JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, MarketDataValueKey](
    db.queryWithResult("SELECT * FROM MarketDataValueKey") { rs =>
      MarketDataValueKey(rs.getInt("id"), Row(rs.getObject[Map[String, Any]]("valueKey").mapKeys(PField(_))))
    }.toMapWithKeys(_.id)))

  log.debug("Loaded %s extended keys, %s value keys" % (extendedKeys.size, valueKeys.size))

  def checkIntegrity(): Unit = {
    /*
    * We cannot (easily?) know we have "duplicates" in the new schema, as the latest value for an observation day's
    * extended key will be taken to be the one having the greatest commitId.
    */
  }

  def readAll(): Unit = {
    throw new Exception("Not implemented")
  }

  def marketDataSetNames(): List[String] = {
    db.queryWithResult("SELECT DISTINCT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds FROM MarketDataExtendedKey ORDER BY mds") {
      rs => rs.getString("mds")
    }
  }

  private val observationDaysByMarketDataSetCache = new SynchronizedVar[MultiMap[String, Day]](db.queryWithResult(
        select("DISTINCT observationDay, marketDataSet")
         from ("MarketDataValue v")
    innerJoin ("MarketDataExtendedKey k", "k.id" eql "v.extendedKey")
        where ("observationDay" isNotNull)) { rs => (rs.getString("marketDataSet"), rs.getDay("observationDay")) }
    .toMultiMap.withDefaultValue(List.empty[Day])
  )

  def observationDaysByMarketDataSet = observationDaysByMarketDataSetCache.get

//  def observationDaysFor(marketDataSets: List[MarketDataSet]): Set[Option[Day]] = db.queryWithResult(
//    select("DISTINCT observationDay")
//      from("MarketDataValue v")
//      innerJoin("MarketDataExtendedKey k", "k.id" eql "v.extendedKey")
//      where("marketDataSet" in marketDataSets.map(_.name))) {
//
//    rs => rs.getDayOption("observationDay")
//  }.toSet

//  def excelObservationDays(): Set[(String, Option[Day])] = db.queryWithResult(
//       select("DISTINCT marketDataSet COLLATE sql_latin1_general_cp1_cs_as as mds, observationDay")
//         from("MarketDataValue v")
//    innerJoin("MarketDataExtendedKey k", "k.id" eql "v.extendedKey")
//        where("marketDataSet" like "Excel:%")) {
//
//    rs => (rs.getString("mds").stripPrefix(MarketDataSet.excelPrefix), rs.getDayOption("observationDay"))
//  }.toSet



  override def latestVersionForAllMarketDataSets() = {
    db.queryWithResult("SELECT extendedKey, MAX(commitid) AS maxCommitId FROM MarketDataValue GROUP BY extendedKey ORDER BY maxCommitId") { rs =>
      (extendedKeys(rs.getInt("extendedKey")).marketDataSet, rs.getInt("maxCommitId"))
    }.toMap
  }

  // TODO [29 Sep 2011] Make quicker (i.e. use extended key cache)
  def latestObservationDaysFor(marketDataSets: List[MarketDataSet], marketDataType: MarketDataType) = {
    db.queryWithOneResult(
       select("MAX(v.observationDay) AS maxObservationDay")
         from("MarketDataValue v")
    innerJoin("MarketDataExtendedKey ek", "ek.id" eql "v.extendedKey")
        where("ek.marketDataType" eql PersistAsBlob(marketDataType))
          and("ek.marketDataSet" in marketDataSets.map(_.name))
    ) { rs => rs.getDay("maxObservationDay") }
  }

  def latestVersionForMarketDataSets(): Map[MarketDataSet, Int] = {
    latestVersionForAllMarketDataSets.filterKeysNot(_.isExcel)

//    Map() ++ db.queryWithResult("""
//      SELECT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds, max(commitId) AS maxCommitId
//        FROM MarketDataExtendedKey ek
//  INNER JOIN MarketDataValue v ON v.extendedKey = ek.id
//       WHERE marketDataSet NOT LIKE 'Excel:%'
//    GROUP BY marketDataSet
//    """, Map()) {
//
//      rs => MarketDataSet.fromName(rs.getString("mds")) → rs.getInt("maxCommitId")
//    }
  }

  def latestExcelVersions(): Map[String, Int] = {
    latestVersionForAllMarketDataSets.filterKeys(_.isExcel).mapKeys(_.name)

//    Map() ++ db.queryWithResult("""
//      SELECT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds, MAX(commitId) AS maxCommitId
//        FROM MarketDataExtendedKey ek
//  INNER JOIN MarketDataValue v ON v.extendedKey = ek.id
//  INNER JOIN MarketDataCommit c ON c.id = v.commitId
//       WHERE marketDataSet like 'Excel:%'
//    GROUP BY marketDataSet
//    """, Map()) {
//
//      rs => rs.getString("mds").stripPrefix("Excel:") → rs.getInt("maxCommitId")
//    }
  }

  def store(marketDataUpdates: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet): SaveResult = {
    var update = false
    var innerMaxVersion = 0//updates.flatMap(_.existingVersion).
    val cacheChanges = ListBuffer[(String, Day)]()

    db.inTransaction(writer => {
      val updates: Iterable[Map[String, Any]] = marketDataUpdates.flatMap { marketDataUpdate =>
        updateIt(writer, marketDataUpdate, marketDataSet).map(result => {
          cacheChanges ++= result.cacheChanges
          if (result.changed) update = true
          innerMaxVersion = scala.math.max(innerMaxVersion, result.version)
          result.updates
        } ).getOrElse(Nil)
      }

      writer.insert("MarketDataValue", updates.toSeq)
    } )

    observationDaysByMarketDataSetCache.update(_ union cacheChanges.toMultiMap)

    SaveResult(innerMaxVersion, update)
  }

  def maxVersionForMarketDataSetNames(names: List[String]): Option[Int] = {
    val namesSet = names.toSet

    latestVersionForAllMarketDataSets.filterKeys(marketDataSet => namesSet.contains(marketDataSet.name)).values.toList.optMax

//    db.queryWithOneResult("""
//        SELECT MAX(commitId) maxCommitId
//          FROM MarketDataCommit c
//    INNER JOIN MarketDataValue v ON v.commitId = c.id
//    INNER JOIN MarketDataExtendedKey ek ON ek.id = v.extendedKey
//         WHERE ek.marketDataSet IN (:mds)
//    """, Map("mds" → names)) {
//      _.getInt("maxCommitId")
//    }
  }

  def marketDataTypes(version: Int, mds: List[MarketDataSet]): Set[MarketDataType] = {
    val sets = mds.toSet

    db.queryWithResult(select("DISTINCT extendedKey") from("MarketDataValue") where("commitId" lte version)) { rs => {
      val extendedKey = extendedKeys(rs.getInt("extendedKey"))

      sets.contains(extendedKey.marketDataSet) ? some(extendedKey.marketDataType) | none[MarketDataType]
    }}.flatten

//    db.queryWithResult(select("DISTINCT ek.marketDataType COLLATE sql_latin1_general_cp1_cs_as AS mdt")
//      .from("MarketDataExtendedKey ek")
//      .innerJoin("MarketDataValue v", "ek.id" eql "v.extendedKey")
//      .where("marketDataSet" in mds.map(_.name))
//      .and("v.commitId" lte version)) {
//      rs => { MarketDataTypes.fromName(rs.getString("mdt")) }
//    }
  }.toSet

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet) = {
//    require(marketDataType.valueFields.size == 1, "Market data type %s has multiple value field keys: %s "
//      % (marketDataType, marketDataType.valueFields))

    val values = db.queryWithResult(
      select("*")
        from("MarketDataValue")
       where("observationDay" gte from, "observationDay" lte to)
         and("extendedKey" in extendedKeyIdsFor(marketDataType, List(marketDataSet))
         and("value" isNotNull))
    ) { marketDataValue(_) }

    values.groupBy(_.timedKey).mapValuesEagerly { valuesForTimeKey => {
      val latestValuesByValueKey = valuesForTimeKey.groupBy(_.valueKey).mapValues(_.maxBy(_.commitId))
      val maxCommitId = latestValuesByValueKey.values.map(_.commitId).maxOr(0)
      val valueRows = latestValuesByValueKey.map { case (valKey, dayValue) => valKey.row + dayValue.row }.toList

      VersionedMarketData(maxCommitId, marketDataType.createValue(valueRows))
    } }
  }

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {

    val mostRecentValues = log.infoWithTime("query.mostRecentValues") {
      val commitClause = "commitId" lte version
      val observationDayClause = observationDays.fold(Clause.optIn("observationDay", _), TrueClause)
      val extendedKeyClause = "extendedKey" in extendedKeyIdsFor(marketDataType, mds, observationTimes, marketDataKeys)

      val values = MarketDataValueMap()

      db.query(select("*")
                 from("MarketDataValue")
                where(commitClause, observationDayClause, extendedKeyClause, "value" isNotNull)
//              orderBy("commitId".asc)
      ) { marketDataValue(_).update(values) }

      values.values
    }

    val timedData: MultiMap[TimedMarketDataKey, Row] =
      mostRecentValues.toList.groupInto(kv => (kv._1._1), kv => (kv._1._2.row + kv._2._2)).mapValues(_.toList)

    timedData.mapValues(marketDataType.createValue(_)).toList
  }

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType) = {
    db.queryWithResult(
      select("distinct observationDay, extendedKey")
        from("MarketDataValue v")
       where("commitId" lte version, "extendedKey" in extendedKeyIdsFor(marketDataType, mds))) { rs =>

      val extendedKey = extendedKeys(rs.getInt("extendedKey"))
      val observationTime = extendedKey.time
      val marketDataKey = extendedKey.marketDataKey
      val observationPoint = rs.getDayOption("observationDay").fold(_.atTimeOfDay(observationTime), ObservationPoint.RealTime)

      TimedMarketDataKey(observationPoint, marketDataKey)
    }
  }.toSet

  def readLatest(id: MarketDataID): Option[VersionedMarketData] = {
    val keys = extendedKeyIdsFor(id.subTypeKey.dataType, List(id.marketDataSet), Some(Set(id.observationPoint.timeOfDay)),
      Some(Set(id.subTypeKey)))

    val values = if (keys.isEmpty) Nil else db.queryWithResult(
      select("*")
        from("MarketDataValue")
       where("observationDay" eql id.observationPoint.day.getOrElse(null), "extendedKey" in keys, "value" isNotNull)
    ) { rs => marketDataValue(rs) }

    if (values.isEmpty) None else {
      val latestValuesByValueKey = values.groupBy(_.valueKey).mapValues(_.maxBy(_.commitId))
      val maxCommitId = latestValuesByValueKey.values.map(_.commitId).maxOr(0)
      val valueRows = latestValuesByValueKey.map { case (valKey, dayValue) => valKey.row + dayValue.row }.toList

      Some(VersionedMarketData(maxCommitId, Option(id.subTypeKey.dataType.createValue(valueRows))))
    }
  }

  case class UpdateResult(changed: Boolean, version: Int, cacheChanges: List[(String, Day)] = Nil, updates: List[Map[String, Any]] = Nil)

  private def updateIt(writer: DBWriter, anUpdate: MarketDataUpdate, marketDataSet: MarketDataSet): Option[UpdateResult] = {
    val id = anUpdate.dataIdFor(marketDataSet)
    val existingData = anUpdate.existingData
    val maybeData = anUpdate.data

    def findOrUpdateExtendedKey(key: MarketDataExtendedKey, commitId: Int): MarketDataExtendedKey =
      extendedKeys.findOrUpdate(_._2.sameValuesAs(key), insertExtendedKey(key) |> (k => (k.id, k)))._2

    def findOrUpdateValueKey(key: MarketDataValueKey): MarketDataValueKey =
      valueKeys.findOrUpdate(_._2.sameValuesAs(key), insertValueKey(key) |> (k => (k.id, k)))._2

    def insertExtendedKey(key: MarketDataExtendedKey) = key.copy(id = insertKey("MarketDataExtendedKey", key.dbMap))
    def insertValueKey(key: MarketDataValueKey) = key.copy(id = insertKey("MarketDataValueKey", key.dbMap))
    def insertKey(table: String, values: Map[String, Any]) = writer.insertAndReturnKey(table, "id", values).asInstanceOf[Int]

    def nextCommitId(): Int = writer.insertAndReturnKey("MarketDataCommit", "id",
      Map("timestamp" → Clock.timestamp, "username" → User.optLoggedOn.map(_.username).orNull)).toInt

    (existingData, maybeData) match {
      case (None, None) => None
      case (Some(VersionedMarketData(version, Some(old))), Some(newData)) if old == newData => Some(UpdateResult(false, version))
      case _ => {
        val commitId = nextCommitId
        val extendedKey = findOrUpdateExtendedKey(id.extendedKey, commitId)
        val observationDay = id.observationPoint.day.getOrElse(null)

        val existingValueKeys = id.subTypeKey.valueKeys(existingData.flatMap(_.data), referenceDataLookup).
          map(findOrUpdateValueKey(_))

        val template = Map("observationDay" → observationDay, "extendedKey" → extendedKey.id, "commitId" → commitId,
                           "value" → null, "uom" → null, "comment" → null)

        // [TODO] 6 Sep 2011: Don't insert if the value + unit hasn't changed
        val inserts = maybeData.map { data => id.extractValues(data, referenceDataLookup).map {
          case (valueKeyWithNoId, uom, value, comment) => findOrUpdateValueKey(valueKeyWithNoId) |> { valueKey =>
            valueKey → (template + ("valueKey" → valueKey.id) + ("value" → value) + ("uom" → uom) + ("comment" → comment))
          } }
        }.getOrElse(Nil).toMap

        val deletes: Map[MarketDataValueKey, Map[String, Any]] = existingValueKeys.toMapWithValues(valueKey =>
          template + ("valueKey" → valueKey.id)) -- inserts.keySet

        //log.info("%s: inserts: %s\ndeletes: %s" % (marketDataSet.name, inserts, deletes.keySet))

        val updates = (deletes ++ inserts).values.toList
        //writer.insert("MarketDataValue", updates)

        Some(UpdateResult(true, commitId, List((extendedKey.marketDataSet.name, observationDay)), updates))
      }
    }
  }

  private def marketDataValue(rs: RichResultSetRow) =
    MarketDataValue(rs.getDay("observationDay"), extendedKeys(rs.getInt("extendedKey")), valueKeys(rs.getInt("valueKey")),
      rs.getDouble("value"), rs.getString("uom"), rs.getStringOption("comment"), rs.getInt("commitId"))

  private def extendedKeyIdsFor(marketDataType: MarketDataType, mds: List[MarketDataSet],
    observationTimes: Option[Set[ObservationTimeOfDay]] = None, marketDataKeys: Option[Set[MarketDataKey]] = None): List[Int] = {

    extendedKeys.filterValues(key => key.marketDataType == marketDataType && mds.contains(key.marketDataSet) &&
      observationTimes.fold(times => times.contains(key.time), true) &&
      marketDataKeys.fold(keys => keys.contains(key.marketDataKey), true)
    ).keys.toList.distinct
  }

  private case class MarketDataValueMap(
    values: MMap[(TimedMarketDataKey, MarketDataValueKey), (MarketDataSet, Row, Int)] = MMap.empty) {

    def update(combinedKey: (TimedMarketDataKey, MarketDataValueKey), combinedValue: (MarketDataSet, Row, Int)) {
      val (_, row, _) = combinedValue

      if (row.isEmpty) values.remove(combinedKey) else log.debugF("Updating latest value: " + (combinedKey, combinedValue)) {
        values.update(combinedKey, combinedValue)
      }
    }
  }

  case class MarketDataValue(observationDay: Day, extendedKey: MarketDataExtendedKey, valueKey: MarketDataValueKey,
                             value: Double, uom: String, comment: Option[String], commitId: Int) {

    def update(map: MarketDataValueMap) = map.values.get(combinedKey) match {
      case Some((oldMarketDataSet, fieldValue, _)) if oldMarketDataSet.priority > marketDataSet.priority =>
        log.debug("Old data: %s has higher priority than: %s" % (fieldValue, this))

      case Some((_, fieldValue, oldCommitId)) if oldCommitId > commitId =>
        log.debug("Old data: %s has higher commitId than: %s" % (fieldValue, this))

      case oldData => log.debugF("Old data: %s has lower commitId (or lower priority) than: %s" % (oldData, this)) {
        map.update(combinedKey, combinedValue)
      }
    }

    val marketDataSet = extendedKey.marketDataSet
    val timedKey = TimedMarketDataKey(ObservationPoint(observationDay, extendedKey.time), extendedKey.marketDataKey)
    val valueFields = extendedKey.marketDataType.valueFields
    val marketDataTypeValueKey: PField = extendedKey.marketDataType.valueFields.head
    val combinedKey = (timedKey, valueKey)
    val combinedValue = (marketDataSet, row, commitId)

    require(comment.isDefined == (valueFields.size == 2), "Inconsistent number of fields (%s) vs values (%s)" %
      (valueFields, List(Some(value), comment).flatten))

    def row = Row(valueFields.head, uom match {
      case "" => Quantity(value, UOM.SCALAR)
      case "%" => Percentage(value)
      case UOM.Parse(unit) => Quantity(value, unit)
      case _ => throw new Exception("Unrecognized uom: " + uom)
    }) +? comment.map(valueFields.tail.head → _)
  }
}

case class MarketDataExtendedKey(id: Int, marketDataSet: MarketDataSet, marketDataType: MarketDataType,
                                 time: ObservationTimeOfDay, marketDataKey: MarketDataKey) {

  lazy val dbMap = Map("marketDataSet" → marketDataSet.name, "marketDataType" → marketDataType.name,
    "observationTime" → time.name, "marketDataKey" → new PersistAsBlob(marketDataKey))

  def sameValuesAs(that: MarketDataExtendedKey) = that.copy(id = id) == this
}

object MarketDataExtendedKey {
  def apply(rs: ResultSetRow): MarketDataExtendedKey =
    MarketDataExtendedKey(rs.getInt("id"), MarketDataSet.fromName(rs.getString("marketDataSet")),
      MarketDataTypes.fromName(rs.getString("marketDataType")), ObservationTimeOfDay.fromName(rs.getString("observationTime")),
      rs.getObject[MarketDataKey]("marketDataKey"))
}