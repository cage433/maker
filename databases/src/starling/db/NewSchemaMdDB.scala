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
import concurrent.SyncVar
import starling.pivot.{Row, Field => PField}
import collection.immutable._
import collection.Iterator
import collection.mutable.{ListBuffer, Map => MMap}


class NewSchemaMdDB(db: DBTrait[RichResultSetRow], dataTypes: MarketDataTypes) extends MdDB with Log {
  val marketDataExtendedKeyHelper = new MarketDataExtendedKeyHelper(dataTypes)
  import marketDataExtendedKeyHelper._

  val extendedKeys = JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, MarketDataExtendedKey](
    db.queryWithResult("SELECT * FROM MarketDataExtendedKey", Map()) { rs => marketDataExtendedKey(rs) }.toMapWithKeys(_.id)))

  val valueKeys = JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, MarketDataValueKey](
    db.queryWithResult("SELECT * FROM MarketDataValueKey", Map()) { rs =>
      MarketDataValueKey(rs.getObject[Map[String, Any]]("valueKey")).copy(id = rs.getInt("id"))
    }.toMapWithKeys(_.id)))

  log.debug("Loaded %s extended keys, %s value keys" % (extendedKeys.size, valueKeys.size))

  def checkIntegrity(): Unit = {
    /*
    * We cannot (easily?) know we have "duplicates" in the new schema, as the latest value for an observation day's
    * extended key will be taken to be the one having the greatest commitId.
    */
  }

  def readAll(): Unit = {

    val extendedKeyTypePairs = db.queryWithResult("select * from MarketDataValue", Map()) { rs => {
      val mdv = marketDataValue(rs)
      (mdv.valueKey.id, mdv.valueKey.row, mdv.extendedKey.marketDataType)
    } }.toSet
    extendedKeyTypePairs.foreach { case (id, row, marketDataType) => {
      if (row.fields != marketDataType.valueKeyFields) {
        println(id + " has incorrect fields. expected: " + marketDataType.valueKeyFields + " got " + row.fields)
        println(id + " => " + valueKeys(id))
      }
    } }

  }

  def marketDataSetNames(): List[String] = {
    db.queryWithResult("SELECT DISTINCT marketDataSet COLLATE sql_latin1_general_cp1_cs_as AS mds FROM MarketDataExtendedKey ORDER BY mds", Map()) {
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

  private def typeFor(marketDataTypeName:MarketDataTypeName) = dataTypes.fromName(marketDataTypeName)

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
    db.queryWithResult("SELECT extendedKey, MAX(commitid) AS maxCommitId FROM MarketDataValue GROUP BY extendedKey ORDER BY maxCommitId", Map()) { rs =>
      (extendedKeys(rs.getInt("extendedKey")).marketDataSet, rs.getInt("maxCommitId"))
    }.toMap
  }

  // TODO [29 Sep 2011] Make quicker (i.e. use extended key cache)
  def latestObservationDaysFor(marketDataSets: List[MarketDataSet], marketDataType: MarketDataTypeName) = {
    db.queryWithOneResult(
       select("MAX(v.observationDay) AS maxObservationDay")
         from("MarketDataValue v")
    innerJoin("MarketDataExtendedKey ek", "ek.id" eql "v.extendedKey")
        where("ek.marketDataType" eql marketDataType.name)
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

  def latestExcelVersions(): Map[MarketDataSet, Int] = {
    latestVersionForAllMarketDataSets.filterKeys(_.isExcel)

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

  private def queryUsingExtendedKeys[T](extendedKeyIds: List[Int], query: Query)(f: RichResultSetRow => T): List[T] = {
    extendedKeyIds.grouped(1900).toList.flatMap { chunk => { //limit is 2000 but use 1900 to leave space for other parameters
      db.queryWithResult(query and("extendedKey" in chunk))(f)
    } }
  }

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataTypeName, marketDataSet: MarketDataSet) = {
//    require(marketDataType.valueFields.size == 1, "Market data type %s has multiple value field keys: %s "
//      % (marketDataType, marketDataType.valueFields))

    val values = queryUsingExtendedKeys(extendedKeyIdsFor(marketDataType, List(marketDataSet)),
      select("*")
        from("MarketDataValue")
       where("observationDay" gte from, "observationDay" lte to)) { marketDataValue(_) }

    values.groupBy(_.timedKey).mapValuesEagerly { valuesForTimeKey => {
      val latestValuesByValueKey = valuesForTimeKey.groupBy(_.valueKey).mapValues(_.maxBy(_.commitId))

      latestValuesByValueKey.filterValues(_.isSave).ifDefined { latestSavesByValueKey => {
        val maxCommitId = latestValuesByValueKey.values.map(_.commitId).maxOr(0)
        val valueRows = latestSavesByValueKey.map { case (valKey, value) => valKey.row :::? value.row }.toList

        VersionedMarketData(maxCommitId, dataTypes.fromName(marketDataType).createValue(valueRows))
      } }
    } }.collectValues { case Some(v) => v }
  }

  def query(version: Int, mds: List[MarketDataSet], marketDataTypeName: MarketDataTypeName,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {

//    log.infoWithTime("query(%d, (%s), %s, %s, %s, %s)" %
//      (version, mds.map(_.name), marketDataType.name, observationDays, observationTimes, marketDataKeys)) {

    val marketDataType = typeFor(marketDataTypeName)

    val mostRecentValues = log.debugWithTime("query.mostRecentValues") {
      val commitClause = "commitId" lte version
      val observationDayClause = observationDays.fold(Clause.optIn("observationDay", _), TrueClause)

      val values = new MarketDataValueMap()

      queryUsingExtendedKeys(extendedKeyIdsFor(marketDataTypeName, mds, observationTimes, marketDataKeys),
         select("*")
           from("MarketDataValue")
          where(commitClause, observationDayClause)
        orderBy("commitId" asc)
      ) { marketDataValue(_).update(values) }

      values.values
    }

    val timedData: MultiMap[TimedMarketDataKey, Row] =
      mostRecentValues.toList.collect { case ((tmdk, mdvk), (_, Some(row), _)) => tmdk → (mdvk.row + row) }.toMultiMap

    timedData.collectValues { case rows if (rows.nonEmpty) => marketDataType.createValue(rows) }.toList
  }

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataTypeName) = {
    db.queryWithResult(
      select("distinct observationDay, extendedKey")
        from("MarketDataValue v, MarketDataExtendedKey ek")
       where("commitId" lte version)
         and("v.extendedKey" eql "ek.id")
         and("ek.marketDataSet" in mds.map(_.name))
         and("ek.marketDataType" eql marketDataType.name.name)) { rs =>

      val extendedKey = extendedKeys(rs.getInt("extendedKey"))
      val observationTime = extendedKey.time
      val marketDataKey = extendedKey.marketDataKey
      val observationPoint = rs.getDayOption("observationDay").fold(_.atTimeOfDay(observationTime), ObservationPoint.RealTime)

      TimedMarketDataKey(observationPoint, marketDataKey)
    }
  }.toSet

  def readLatest(id: MarketDataID): Option[VersionedMarketData] = {
    val keys = extendedKeyIdsFor(id.subTypeKey.typeName, List(id.marketDataSet), Some(Set(id.observationPoint.timeOfDay)),
      Some(Set(id.subTypeKey)))

    val values = if (keys.isEmpty) Nil else queryUsingExtendedKeys(keys,
      select("*")
        from("MarketDataValue")
       where("observationDay" eql id.observationPoint.day.getOrElse(null))
    ) { rs => marketDataValue(rs) }

    val latestValuesByValueKey = values.groupBy(_.valueKey).mapValues(_.maxBy(_.commitId))
    val filteredLatestValues = latestValuesByValueKey.filter(_._2.isSave)
    if (filteredLatestValues.isEmpty) {
      None
    } else {
      val maxCommitId = filteredLatestValues.values.map(_.commitId).maxOr(0)
      val valueRows = filteredLatestValues.map { case (valKey, dayValue) => valKey.row + dayValue.row.get }.toList

      Some(VersionedMarketData(maxCommitId, Option(dataTypes.fromName(id.subTypeKey.typeName).createValue(valueRows))))
    }
  }

  case class UpdateResult(changed: Boolean, version: Int, cacheChanges: List[(String, Day)] = Nil, updates: List[Map[String, Any]] = Nil)

  private def updateIt(writer: DBWriter, anUpdate: MarketDataUpdate, marketDataSet: MarketDataSet): Option[UpdateResult] = {
    val id = anUpdate.dataIdFor(marketDataSet, dataTypes)
    val existingData = anUpdate.existingData
    val maybeData = anUpdate.data

    def findOrUpdateExtendedKey(key: MarketDataExtendedKey, commitId: Int): MarketDataExtendedKey =
      extendedKeys.findOrUpdate(_._2.sameValuesAs(key), insertExtendedKey(key) |> (k => (k.id, k)))._2

    def findOrUpdateValueKey(key: MarketDataValueKey): MarketDataValueKey =
      valueKeys.findOrUpdate(_._2.sameValuesAs(key), insertValueKey(key) |> (k => (k.id, k)))._2

    def insertExtendedKey(key: MarketDataExtendedKey) = key.copy(id = insertKey("MarketDataExtendedKey", key.dbMap))
    def insertValueKey(key: MarketDataValueKey) = {
      key.row.fields.requireEqual(dataTypes.fromName(anUpdate.timedKey.typeName).valueKeyFields)
      key.copy(id = insertKey("MarketDataValueKey", key.dbMap))
    }
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

        val existingValueKeys = existingData.flatMap(_.data).fold(id.valueKeys(_), Nil).map(findOrUpdateValueKey(_))

        val template = Map("observationDay" → observationDay, "extendedKey" → extendedKey.id, "commitId" → commitId,
                           "value" → null, "uom" → null, "comment" → null)

        // [TODO] 6 Sep 2011: Don't insert if the value + unit hasn't changed
        val inserts = maybeData.map { data => id.extractValues(data).map {
          case (valueKeyWithNoId, uom, value, comment) => findOrUpdateValueKey(valueKeyWithNoId) |> { valueKey =>
            valueKey → (template + ("valueKey" → valueKey.id) + ("value" → value) + ("uom" → uom) + ("comment" → comment))
          } }
        }.getOrElse(Nil).toMap

        val deletes: NestedMap[MarketDataValueKey, String, Any] = existingValueKeys.toMapWithValues(valueKey =>
          template + ("valueKey" → valueKey.id)) -- inserts.keySet

        //log.info("%s: inserts: %s\ndeletes: %s" % (marketDataSet.name, inserts, deletes.keySet))

        val updates = (deletes ++ inserts).values.toList
        //writer.insert("MarketDataValue", updates)

        Some(UpdateResult(true, commitId, List((extendedKey.marketDataSet.name, observationDay)), updates))
      }
    }
  }

  private def marketDataValue(rs: RichResultSetRow):MarketDataValue =
    MarketDataValue(rs.getDayOption("observationDay"), extendedKeys(rs.getInt("extendedKey")), valueKeys(rs.getInt("valueKey")),
      rs.getDouble("value"), rs.getString("uom"), rs.getStringOption("comment"), rs.getInt("commitId"))

  private def extendedKeyIdsFor(marketDataType: MarketDataTypeName, mds: List[MarketDataSet],
    observationTimes: Option[Set[ObservationTimeOfDay]] = None, marketDataKeys: Option[Set[MarketDataKey]] = None): List[Int] = {

    extendedKeys.filterValues(key => key.marketDataType == dataTypes.fromName(marketDataType) && mds.contains(key.marketDataSet) &&
      observationTimes.fold(times => times.contains(key.time), true) &&
      marketDataKeys.fold(keys => keys.contains(key.marketDataKey), true)
    ).keys.toList.distinct
  }

  private class MarketDataValueMap(
    val values: MMap[(TimedMarketDataKey, MarketDataValueKey), (MarketDataSet, Option[Row], Int)] = MMap.empty) {

    def update(combinedKey: (TimedMarketDataKey, MarketDataValueKey), combinedValue: (MarketDataSet, Option[Row], Int)) {
      val (_, row, _) = combinedValue

      if (row.isEmpty) values.remove(combinedKey) else log.debugF("Updating latest value: " + (combinedKey, combinedValue)) {
        values.update(combinedKey, combinedValue)
      }
    }
  }

  case class MarketDataValue(observationDay: Option[Day], extendedKey: MarketDataExtendedKey, valueKey: MarketDataValueKey,
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
    def checkConsistency() = {
      if (extendedKey.marketDataType.valueKeyFields == valueKey.fields) None else {
        Some("" + extendedKey.marketDataType.valueKeyFields + " != x" + valueKey.fields)
      }
    }
    def isSave = row.isDefined
    val marketDataSet = extendedKey.marketDataSet
    val timedKey = TimedMarketDataKey(observationDay.fold(d => ObservationPoint(d, extendedKey.time), ObservationPoint.RealTime), extendedKey.marketDataKey)
    val valueFields = extendedKey.marketDataType.valueFields
    val marketDataTypeValueKey: PField = extendedKey.marketDataType.valueFields.head
    val combinedKey = (timedKey, valueKey)
    val combinedValue = (marketDataSet, row, commitId)


    lazy val row = if (value==null || uom == null) {
      None
    } else {
      require(comment.isDefined == (valueFields.size == 2), "Inconsistent number of fields (%s) vs values (%s)" %
        (valueFields, List(Some(value), comment).flatten))
      Some(Row(valueFields.head, uom match {
        case "" => Quantity(value, UOM.SCALAR)
        case "%" => Percentage(value)
        case UOM.Parse(unit) => Quantity(value, unit)
        case _ => throw new Exception("Unrecognized uom: " + uom + " in row " + timedKey + " " + valueFields + " " + value)
      }) +? comment.map(valueFields.tail.head → _))
    }
  }
}

case class MarketDataExtendedKey(id: Int, marketDataSet: MarketDataSet, marketDataType: MarketDataType,
                                 time: ObservationTimeOfDay, marketDataKey: MarketDataKey) {

  lazy val dbMap = Map("marketDataSet" → marketDataSet.name, "marketDataType" → marketDataType.name.name,
    "observationTime" → time.name, "marketDataKey" → new PersistAsBlob(marketDataKey))

  def sameValuesAs(that: MarketDataExtendedKey) = that.copy(id = id) == this
}

class MarketDataExtendedKeyHelper(marketDataTypes: MarketDataTypes) {
  def marketDataExtendedKey(rs: ResultSetRow): MarketDataExtendedKey =
    MarketDataExtendedKey(rs.getInt("id"), MarketDataSet.fromName(rs.getString("marketDataSet")),
      marketDataTypes.fromName(rs.getString("marketDataType")), ObservationTimeOfDay.fromName(rs.getString("observationTime")),
      rs.getObject[MarketDataKey]("marketDataKey"))
}
