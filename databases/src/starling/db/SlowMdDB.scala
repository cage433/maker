package starling.db

import starling.dbx._
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

import starling.utils.Pattern._
import java.lang.String
import java.util.concurrent.atomic.AtomicInteger
import starling.utils._
import collection.immutable.{Map, TreeMap}
import scalaz._
import Scalaz._
import collection.immutable.{Iterable, Map, TreeMap}
import collection.JavaConversions._
import collection.mutable.{HashMap, SetBuilder, ListBuffer, HashSet => MSet}
import starling.instrument.utils.StarlingXStream
import starling.pivot.{Row, PivotQuantity, PivotEdits, PivotTableDataSource, Field => PField}

//import starling.props.Props.VarReportEmailFrom

// TODO [07 Sep 2010] move me somewhere proper
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

  def observationDaysByMarketDataSet = db.queryWithResult((select("distinct observationDay, marketDataSet") from "MarketData") where("observationDay" isNotNull)) {
    rs => (rs.getString("marketDataSet"), rs.getDay("observationDay"))
  }.toMultiMap.withDefaultValue(Set.empty[Day])

  def latestVersionForMarketDataSets(): Map[MarketDataSet, Int] = {
    Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet not like 'Excel:%' group by marketDataSet ", Map()) {
      rs => MarketDataSet.fromName(rs.getString("marketDataSet")) -> rs.getInt("m")
    }
  }

  def latestObservationDaysFor(marketDataSets: List[MarketDataSet], marketDataType: MarketDataType) = db.queryWithOneResult(
       select("max(md.observationDay) as maxObservationDay")
         from("MarketData md")
        where("md.marketDataType" eql PersistAsBlob(marketDataType))
          and("md.marketDataSet" in marketDataSets.map(_.name)))
    { rs => rs.getDay("maxObservationDay") }

  def latestExcelVersions(): Map[String, Int] = {
    Map() ++ db.queryWithResult("select marketDataSet, max(version) m from MarketData where marketDataSet like 'Excel:%' group by marketDataSet ", Map()) {
      rs => rs.getString("marketDataSet").stripPrefix("Excel:") → rs.getInt("m")
    }
  }

  def store(data: Iterable[MarketDataUpdate], marketDataSet: MarketDataSet): SaveResult = {
    var update = false
    var innerMaxVersion = 0

    db.inTransaction(dbWriter => data.map { action =>
      updateIt(dbWriter, action.dataIdFor(marketDataSet), action.existingData, action.data).foreach {
        result =>
          if (result._1) update = true
          innerMaxVersion = scala.math.max(innerMaxVersion, result._2)
      }
    } )

    SaveResult(innerMaxVersion, update)
  }

  private def updateIt(dbWriter: DBWriter, id: MarketDataID, existingData: Option[VersionedMarketData],
                       maybeData: Option[MarketData]): Option[(Boolean, Int)] = {

    val timestamp = Clock.timestamp

    (existingData, maybeData) match {
      case (None, None) => None
      case (Some(VersionedMarketData(v, Some(d))), Some(data)) if d == data => Some((false, v))
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

  def marketDataTypes(version: Int, mds: List[MarketDataSet]): Set[MarketDataType] = {
    queryForMarketDataIdentifier(
      version, mds,
      "distinct marketDataType",
      None,
      rs => rs.getObject[MarketDataType]("marketDataType")
    )
  }.toSet

  def latestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet) = {
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
    results.toMap
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

  def queryForObservationDayAndMarketDataKeys(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType): Set[TimedMarketDataKey] = {
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
  }.toSet

  private def typeAndSnapshotClauses(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType): Clause = {
    (("marketDataSet" in mds.map(_.name))
      and (("version" lte version) and ((("childVersion") isNull) or ("childVersion" gt version)))
      and ("marketDataType" eql LiteralString(StarlingXStream.write(marketDataType))))
  }

  def query(version: Int, mds: List[MarketDataSet], marketDataType: MarketDataType,
            observationDays: Option[Set[Option[Day]]], observationTimes: Option[Set[ObservationTimeOfDay]],
            marketDataKeys: Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {

    val observationDayClause = observationDays.map(Clause.optIn("observationDay", _))
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
      val allDataForKeyAndDay: List[Row] = mds.reverse.flatMap(mds => {
        latestValues.get(mds.name).toList.flatMap(data => key.castRows(data, ReferenceDataLookup.Null))
      })
      MarketDataStore.applyOverrideRule(marketDataType, allDataForKeyAndDay.map(_.value)).ifDefined { rowsFromOneMap =>
        val marketData = marketDataType.createValue(Row.create(rowsFromOneMap))
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
    new VersionedMarketData(rs.getInt("version"), rs.getObjectOption[Any]("data").map(key.unmarshallDB(_)))
  }
}
