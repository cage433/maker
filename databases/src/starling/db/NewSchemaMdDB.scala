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
import collection.immutable._
import starling.richdb.RichResultSetRow
import starling.pivot.{Field => PField}
import collection.mutable.{ListBuffer, Map => MMap}
import concurrent.SyncVar


class NewSchemaMdDB(db: DBTrait[RichResultSetRow]) extends MdDB with Log {
  val extendedKeys = JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, MarketDataExtendedKey](
    db.queryWithResult("SELECT * FROM MarketDataExtendedKey") { rs => MarketDataExtendedKey(rs) }.toMapWithKeys(_.id)))

  val valueKeys = JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, MarketDataValueKey](
    db.queryWithResult("SELECT * FROM MarketDataValueKey") { rs =>
      MarketDataValueKey(rs.getInt("id"), rs.getObject[Map[String, Any]]("valueKey").mapKeys(PField(_)))
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

  class RichSyncVar[A](initial: => A) extends SyncVar[A] {
    put(initial)

    def update(f: (A) => A) = synchronized { put(f(get)) }
  }

  private val observationDaysByMarketDataSetCache = new RichSyncVar[Map[String, Set[Day]]](db.queryWithResult(
        select("DISTINCT observationDay, marketDataSet")
         from ("MarketDataValue v")
    innerJoin ("MarketDataExtendedKey k", "k.id" eql "v.extendedKey")
        where ("observationDay" isNotNull)) { rs => (rs.getString("marketDataSet"), rs.getDay("observationDay")) }
    .toMultiMap.withDefaultValue(Set.empty[Day])
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

  def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet): SaveResult = {
    var update = false
    var innerMaxVersion = 0
    val cacheChanges = ListBuffer[(String, Day)]()

    db.inTransaction(writer => data.map { action =>
      updateIt(writer, action.dataIdFor(marketDataSet), action.existingData, action.data).foreach { result =>
        cacheChanges ++= result.cacheChanges
        if (result.changed) update = true
        innerMaxVersion = scala.math.max(innerMaxVersion, result.version)
      }
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
    require(marketDataType.valueFields.size == 1, "Market data type %s has multiple value field keys: %s "
      % (marketDataType, marketDataType.valueFields))

    val values = db.queryWithResult(
      select("v.observationDay", "v.extendedKey", "v.valueKey", "v.value", "v.uom, v.commitId")
        from("MarketDataValue v")
       where("v.observationDay" gte from, "v.observationDay" lte to)
         and("v.extendedKey" in extendedKeyIdsFor(marketDataType, List(marketDataSet)))
    ) { marketDataValue(_) }

    values.groupBy(_.timedKey).mapValuesEagerly { valuesForTimeKey => {
      val latestValuesByValueKey = valuesForTimeKey.groupBy(_.valueKey).mapValues(_.maxBy(_.commitId)).filterValuesNot(_.isDelete)
      val maxCommitId = latestValuesByValueKey.values.map(_.commitId).maxOr(0)
      val valueMaps = latestValuesByValueKey.map { case (valKey, dayValue) => valKey.value + dayValue.fieldValue }.toList

      try {
        VersionedMarketData(maxCommitId, marketDataType.createValue(valueMaps))
      } catch {
        case e => throw e
      }
    } }
  }

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {

    val mostRecentValues = log.infoWithTime("query.mostRecentValues") {
      val commitClause = "v.commitId" lte version
      val observationDayClause = observationDays.fold(Clause.optIn("v.observationDay", _), TrueClause)
      val extendedKeyClause = "v.extendedKey" in extendedKeyIdsFor(marketDataType, mds, observationTimes, marketDataKeys)

      val values = MarketDataValueMap()

      db.query(select("v.observationDay", "v.extendedKey", "v.valueKey", "v.value", "v.uom, v.commitId")
                 from("MarketDataValue v")
                where(commitClause, observationDayClause, extendedKeyClause)
//              orderBy("commitId".asc)
      ) { marketDataValue(_).update(values) }

      values.values
    }

    val timedData: Map[TimedMarketDataKey, List[Map[PField, Any]]] =
      mostRecentValues.toList.groupInto(kv => (kv._1._1), kv => (kv._1._2.value + kv._2._2)).mapValues(_.toList)

    timedData.mapValues(marketDataType.createValue(_)).toList
  }

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType) = {
    db.queryWithResult(
      select("distinct v.observationDay, v.extendedKey")
        from("MarketDataValue v")
       where("v.commitId" lte version, "v.extendedKey" in extendedKeyIdsFor(marketDataType, mds))) { rs =>

      val extendedKey = extendedKeys(rs.getInt("extendedKey"))
      val observationTime = extendedKey.time
      val marketDataKey = extendedKey.marketDataKey

      val observationPoint = rs.getDayOption("observationDay").
        fold(observationDay => observationDay.atTimeOfDay(observationTime), ObservationPoint.RealTime)

      TimedMarketDataKey(observationPoint, marketDataKey)
    }
  }.toSet

  def readLatest(id: MarketDataID): Option[VersionedMarketData] = {
    val keys = extendedKeyIdsFor(id.subTypeKey.dataType, List(id.marketDataSet), Some(Set(id.observationPoint.timeOfDay)),
      Some(Set(id.subTypeKey)))

    val values = if (keys.isEmpty) Nil else db.queryWithResult(
      select("*")
        from("MarketDataValue v")
       where("v.observationDay" eql id.observationPoint.day.getOrElse(null), "v.extendedKey" in keys)
    ) { rs => marketDataValue(rs) }

    if (values.isEmpty) None else {
      val latestValuesByValueKey = values.groupBy(_.valueKey).mapValues(_.maxBy(_.commitId))
      val maxCommitId = latestValuesByValueKey.values.map(_.commitId).maxOr(0)
      val valueMaps = latestValuesByValueKey.map { case (valKey, dayValue) => valKey.value + dayValue.fieldValue }.toList

      Some(VersionedMarketData(maxCommitId, Option(id.subTypeKey.dataType.createValue(valueMaps))))
    }
  }

  case class UpdateResult(changed: Boolean, version: Int, cacheChanges: List[(String, Day)] = Nil)

  private def updateIt(writer: DBWriter, id: MarketDataID, existingData: Option[VersionedMarketData],
                       maybeData: Option[MarketData]): Option[UpdateResult] = {

    def findOrUpdateExtendedKey(key: MarketDataExtendedKey, commitId: Int): MarketDataExtendedKey =
      extendedKeys.findOrUpdate(_._2.sameValuesAs(key), insertExtendedKey(key).apply(k => (k.id, k)))._2

    def findOrUpdateValueKey(key: MarketDataValueKey): MarketDataValueKey =
      valueKeys.findOrUpdate(_._2.sameValuesAs(key), insertValueKey(key).apply(k => (k.id, k)))._2

    def insertExtendedKey(key: MarketDataExtendedKey) = key.copy(id = insertKey("MarketDataExtendedKey", key.dbMap))
    def insertValueKey(key: MarketDataValueKey) = key.copy(id = insertKey("MarketDataValueKey", key.dbMap))
    def insertKey(table: String, values: Map[String, Any]) = writer.insertAndReturnKey(table, "id", values).asInstanceOf[Int]

    def nextCommitId(): Int = writer.insertAndReturnKey("MarketDataCommit", "id",
      Map("timestamp" → Clock.timestamp, "username" → User.optLoggedOn.map(_.username).orNull)).toInt

    (existingData, maybeData) match {
      case (None, None) => None
      case (Some(VersionedMarketData(version, Some(oldData))), Some(newData)) if oldData == newData => Some(UpdateResult(false, version))
      case _ => {
        val commitId = nextCommitId
        val extendedKey = findOrUpdateExtendedKey(id.extendedKey, commitId)
        val observationDay = id.observationPoint.day.getOrElse(null)
        val template = Map("observationDay" → observationDay, "extendedKey" → extendedKey.id, "commitId" → commitId)
        val existingValueKeys = id.subTypeKey.valueKeys(existingData.flatMap(_.data))

        val deletes: Map[MarketDataValueKey, Map[String, Any]] = existingValueKeys.toMapWithValues { valueKey =>
          template + ("valueKey" → valueKey) + ("value" → null) + ("uom" → null)
        }

        // [TODO] 6 Sep 2011: Don't insert if the value + unit hasn't changed
        val inserts: Map[MarketDataValueKey, Map[String, Any]] = maybeData match {
          case None => Map.empty[MarketDataValueKey, Map[String, Any]]
          case Some(data) => id.extractValues(data).map { case (valueKey, uom, value) =>
            valueKey → (template + ("valueKey" → findOrUpdateValueKey(valueKey).id) + ("value" → value) + ("uom" → uom))
          }.toMap
        }

        writer.insert("MarketDataValue", (deletes ++ inserts).values.toSeq)

        Some(UpdateResult(true, commitId, List((extendedKey.marketDataSet.name, observationDay))))
      }
    }
  }

  private def marketDataValue(rs: RichResultSetRow) =
    MarketDataValue(rs.getDay("observationDay"), extendedKeys(rs.getInt("extendedKey")), valueKeys(rs.getInt("valueKey")),
      rs.getDouble("value"), rs.getString("uom"), rs.getInt("commitId"))

  private def extendedKeyIdsFor(marketDataType: MarketDataType, mds: List[MarketDataSet],
    observationTimes: Option[Set[ObservationTimeOfDay]] = None, marketDataKeys: Option[Set[MarketDataKey]] = None): List[Int] = {

    extendedKeys.filterValues(key => key.marketDataType == marketDataType && mds.contains(key.marketDataSet) &&
      observationTimes.fold(times => times.contains(key.time), true) &&
      marketDataKeys.fold(keys => keys.contains(key.marketDataKey), true)
    ).keys.toList.distinct
  }

  private case class MarketDataValueMap(
    values: MMap[(TimedMarketDataKey, MarketDataValueKey), (MarketDataSet, (PField, Any), Int)] = MMap.empty) {

    def update(combinedKey: (TimedMarketDataKey, MarketDataValueKey), combinedValue: (MarketDataSet, (PField, Any), Int)) {
      val (_, (_, value), _) = combinedValue

      if (value == null) values.remove(combinedKey) else log.debugF("Updating latest value: " + (combinedKey, combinedValue)) {
        values.update(combinedKey, combinedValue)
      }
    }
  }

  case class MarketDataValue(observationDay: Day, extendedKey: MarketDataExtendedKey, valueKey: MarketDataValueKey,
                                     value: Double, uom: String, commitId: Int) {

    def update(map: MarketDataValueMap) = map.values.get(combinedKey) match {
      case Some((oldMarketDataSet, fieldValue, _)) if oldMarketDataSet.priority > marketDataSet.priority =>
        log.debug("Old data: %s has higher priority than: %s" % (fieldValue, this))

      case Some((_, fieldValue, oldCommitId)) if oldCommitId > commitId =>
        log.debug("Old data: %s has higher commitId than: %s" % (fieldValue, this))

      case oldData => log.debugF("Old data: %s has lower commitId (or lower priority) than: %s" % (oldData, this)) {
        map.update(combinedKey, combinedValue)
      }
    }

    val isDelete = uom == null
    val marketDataSet = extendedKey.marketDataSet
    val timedKey = TimedMarketDataKey(ObservationPoint(observationDay, extendedKey.time), extendedKey.marketDataKey)
    val marketDataTypeValueKey: PField = extendedKey.marketDataType.valueFields.head
    val combinedKey = (timedKey, valueKey)
    val combinedValue = (marketDataSet, fieldValue, commitId)

    // Should these just be creating MarketValues ?
    def fieldValue: (PField, Any) = (marketDataTypeValueKey, (uom ?? "").trim match {
      case "" => null
      case "%" => Percentage(value)
      case UOM.Parse(unit) => Quantity(value, unit)
      case _ => throw new Exception("Unrecognized uom: " + uom)
    })
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