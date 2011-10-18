package starling.schemaevolution

import starling.db._
import starling.utils._
import starling.utils.ImplicitConversions._
import java.lang.String
import collection.immutable.Map
import starling.utils.ClosureUtil._
import starling.curves.SpreadStdDevSurfaceData
import starling.quantity.{UOM, Quantity, Percentage}
import starling.daterange.{Day, SpreadPeriod, Period, ObservationTimeOfDay}
import scala.Any
import collection.mutable.{Map => MMap}
import java.util.concurrent.ConcurrentMap
import scalaz.Scalaz._
import collection.Iterable
import starling.marketdata._
import starling.instrument.utils.StarlingXStream
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.props.PropsHelper
import starling.pivot.{Row, Field, PivotQuantity}
import system.{PatchContext, Patch}


class Patch132_MigrateMarketDataToFasterSchema extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) =
    MigrateMarketDataSchema(writer, starling.db).migrateData
}

class Patch120_MakeVersionNullableInMarketDataComment extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) =
    writer.update("ALTER TABLE MarketDataCommit ALTER COLUMN version int NULL")
}

class Patch120_Add_Comment_To_MarketDataValue extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) =
    writer.update("ALTER TABLE MarketDataValue ADD comment varchar(128) NULL")
}

object MigrateMarketDataSchema extends Log {
  def main(args: Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")

    val props = PropsHelper.defaultProps

    val init = new StarlingInit(props)

    log.infoWithTime("Migrating data") {
      init.starlingDB.inTransaction(writer => MigrateMarketDataSchema(writer, init.starlingDB).migrateData)
    }
  }
}

case class MigrateMarketDataSchema(writer: DBWriter, db: DB) extends Log {
  private val tableNameMarketDataCommit = "MarketDataCommit"
  private val tableNameMarketDataExtendedKey = "MarketDataExtendedKey"
  private val tableNameMarketDataValue = "MarketDataValue"
  private val tableNameMarketDataValueKey = "MarketDataValueKey"
  private val tableNameMarketDataTag = "MarketDataTag"

  private val alterTableMarketDataTagSql = "alter table [dbo].%s add commitId int".format(tableNameMarketDataTag)

  private val createTableMarketDataCommitSql = """
  create table [dbo].""" + tableNameMarketDataCommit + """ (
    id            int IDENTITY(1,1) NOT NULL
    , version     int NOT NULL
    , timestamp   datetime
    , username    varchar(128)
  )
  """

  private val createTableMarketDataExtendedKeySql = """
  create table [dbo].""" + tableNameMarketDataExtendedKey + """ (
    id                int IDENTITY(1,1) NOT NULL
    , marketDataSet   varchar(128)
    , marketDataType  varchar(128)
    , observationTime varchar(60)
    , marketDataKey   text
    )
  """

  private val createTableMarketDataValueKeySql = """
  create table [dbo].""" + tableNameMarketDataValueKey + """ (
    id      int IDENTITY(1,1) NOT NULL
    , valueKey text
  )
  """

  private val createTableMarketDataValueSql = """
  create table [dbo].""" + tableNameMarketDataValue + """ (
    observationDay  date
    , extendedKey   int
    , valueKey      int
    , value         decimal(19,8)
    , uom           varchar(12)
    , commitId      int
  )
  """

  private val extendedKeys = MMap.empty[MarketDataExtendedKey, Int]
  private val valueKeys = MMap.empty[MarketDataValueKey, Int]

  object RichConcurrencyMap {
    implicit def enrichConcurrentMap[K, V](map: ConcurrentMap[K, V]) = new RichConcurrencyMap(map)

    class RichConcurrencyMap[K, V](map: ConcurrentMap[K, V]) {
      def memoize(key: K)(f: => V): V = map.containsKey(key) ? map.get(key) | map.putIfAbsent(key, f)
    }
  }

  private val previousInsertsByChildVersion = MMap.empty[Int, Map[MarketDataValueKey, (Any, Any)]]

  private val (noUnit, noValue) = (null.asInstanceOf[Any], null.asInstanceOf[Any])

  private val insertBuffer = new scala.collection.mutable.ArrayBuffer[Map[String, Any]]() {
    override def append(elems: Map[String, Any]*) {
      super.append(elems : _*)
      if (size > 2000) writeContents
    }

    def writeContents = if (!isEmpty) {
      writer.insert(tableNameMarketDataValue, toList)
      clear
    }
  }

  def migrateData {
    val minVersion = migrateSchema
    val maxVersion = db.queryWithOneResult("SELECT max(version) as maxVersion from MarketData") { rs => rs.getInt("maxVersion") }
      .getOrElse(-1)

    log.info("Mapping MarketData (maxVersion: %s) to MarketDataValue..." % maxVersion)
    db.query("select * from MarketData where version >= %d order by version" % (minVersion)) { rs => {
      val observationDay = rs.getDayOption("observationDay").getOrElse(null)
      val marketDataSet = MarketDataSet.fromName(rs.getString("marketDataSet"))

      val marketDataType = rs.getObject[MarketDataType]("marketDataType")
      val key = rs.getObject[MarketDataKey]("marketDataKey")
      val version = rs.getInt("version")

      val extendedKey = MarketDataExtendedKey(-1, marketDataSet, marketDataType,
        ObservationTimeOfDay.fromName(rs.getString("observationTime")), key)

      val commitId = writer.insertAndReturnKey("MarketDataCommit", "id",
        Map("timestamp" → rs.getTimestamp("timestamp"), "version" → version))

      if ((rs.count % 100) == 0)
        log.info("row: %d, version: %d (of %d), cache.size: %d" %
          (rs.count, version, maxVersion, previousInsertsByChildVersion.size))

      updateMarketDataTag(commitId, version)

      val inserts: Map[MarketDataValueKey, (Any, Any)] = previousInsertsByChildVersion.remove(version) match {
        case None => {
          val inserts: Map[MarketDataValueKey, (Any, Any)] = rs.getStringOption("data").map { xml =>
            readRows(xml, key, version).flatMap { row => {
              val valueKey = getValueKey(key, row)

              getUOMValueOption(key, row) flatMap { uomValue => Some(valueKey → uomValue) }
            } }
          }.getOrElse(Nil).toMap

          inserts
        }
        case Some(previousInserts) => {
          val deletes: MMap[MarketDataValueKey, (Any, Any)] = previousInserts.mapValues { case _ => (noUnit, noValue) }.mutable

          val inserts: Map[MarketDataValueKey, (Any, Any)] = rs.getStringOption("data").map { xml =>
            readRows(xml, key, version).flatMap { row => {
              val valueKey = getValueKey(key, row)

              getUOMValueOption(key, row) flatMap {
                case uomValue if (previousInserts.get(valueKey) == Some(uomValue)) => { deletes.remove(valueKey); None }
                case uomValue => Some(valueKey → uomValue)
              }
            } }
          }.getOrElse(Nil).toMap

          (deletes.toMap ++ inserts)
        }
      }

      rs.getIntOption("childVersion").foreach { previousInsertsByChildVersion(_) = inserts }

      inserts.foreach { case (valueKey, (uom, value)) => insertBuffer.append(Map(
        "observationDay" → observationDay, "extendedKey" → idFor(extendedKey), "commitId" → commitId,
        "valueKey" → idFor(valueKey), "value" → value, "uom" → uom
      )) }
    } }

    log.info("final cache.size = " + previousInsertsByChildVersion.size)

    insertBuffer.writeContents

    db.queryWithOneResult("select count(*) as nullRowCount from " + tableNameMarketDataTag + " where commitId is null") { rs => {
      log.info(rs.getLong("nullRowCount") + " MarketDataTag row(s) have no commit Id")
    } }

    writer.updateMany(
      "CREATE INDEX MarketDataValue_ObservationDay ON MarketDataValue (observationDay)",
      "CREATE INDEX MarketDataValue_ExtendedKey ON MarketDataValue (extendedKey)",
      "CREATE INDEX MarketDataValue_CommitID ON MarketDataValue (commitId)",
      "CREATE INDEX MarketDataValue_ExtendedKey_ObservationDay ON MarketDataValue (extendedKey, observationDay)",
      "CREATE INDEX MarketDataValue_ExtendedKey_CommitId ON MarketDataValue (extendedKey, commitId)",
      "CREATE INDEX MarketDataExtendedKey_marketDataSet\nON MarketDataExtendedKey (marketDataSet)"
    )
  }

  private def updateMarketDataTag(commitId: Long, version: Int) {
    val rows = writer.update("UPDATE [dbo].%s SET commitId=%d WHERE version=%d" % (tableNameMarketDataTag, commitId, version))

    if (rows > 0) log.info("Updated %d row(s) for commitId: %d to version: %d\n" % (rows, commitId, version))
  }

  private def migrateSchema: Int = {
    testReadingOfCleanedUpXml

    val minVersion = 0

    if (minVersion == 0) {
      log.info("Dropping tables...")
      List(tableNameMarketDataValue, tableNameMarketDataCommit, tableNameMarketDataExtendedKey, tableNameMarketDataValueKey)
        .foreach { table => safely { writer.update("drop table " + table) } }

      log.info("Creating tables...")
      writer.update(alterTableMarketDataTagSql)
      writer.update("update " + tableNameMarketDataTag + " set commitId=null")
      writer.updateMany(createTableMarketDataExtendedKeySql, createTableMarketDataValueKeySql, createTableMarketDataValueSql,
        createTableMarketDataCommitSql)
    } else {
      extendedKeys ++= db.queryWithResult("SELECT * FROM MarketDataExtendedKey") { MarketDataExtendedKey(_) }.toMapWithValues(_.id)
      valueKeys ++= db.queryWithResult("SELECT * FROM MarketDataValueKey") { rs =>
        MarketDataValueKey(rs.getInt("id"), Row(rs.getObject[Map[String, Any]]("valueKey").mapKeys(Field(_))))
      }.toMapWithValues(_.id)
    }

    minVersion
  }

  private def idFor(key: MarketDataExtendedKey) = extendedKeys.getOrElseUpdate(key, {
    writer.insertAndReturnKey("MarketDataExtendedKey", "id", key.dbMap).toInt
  })

  private def idFor(key: MarketDataValueKey) = valueKeys.getOrElseUpdate(key, {
    writer.insertAndReturnKey("MarketDataValueKey", "id", key.dbMap).toInt
  })

  private def getUOMValueOption(key: MarketDataKey, row: Row): Option[(String, Double)] = {
    val values: List[Any] = getValues(key, row)

    val uomValueOption: Option[(String, Double)] = values match {
      case Nil => log.info("Nil " + key); None
      case one :: Nil => {
        one match {
          case q: Quantity => Some((q.uom.toString, q.value))
          case pq: PivotQuantity if pq.quantityValue.isDefined => Some((pq.quantityValue.get.uom.toString, pq.quantityValue.get.value))
          case pc: Percentage => Some(("%", pc.value))
          case other => log.info("unexpected value " + other.asInstanceOf[AnyRef].getClass + "" + other); None
        }
      }
      case many => log.info("Many " + key + "" + many); None
    }

    uomValueOption
  }

  private def getValues(key: MarketDataKey, row: Row): List[Any] = {
    key.dataType.valueFields.toList.flatMap(f => row.get[Any](f))
  }

  private def readRows(xml: String, key: MarketDataKey, version: Int): Iterable[Row] = {
    val refactoredData = try {
      StarlingXStream.read(cleanUpXml(xml))
    } catch {
      case e => log.fatal(xml); log.fatal("broken version: " + version); throw e
    }

    key.dataType.castRows(key, key.unmarshallDB(refactoredData), ReferenceDataLookup.Null)
  }

  private def getValueKey(key: MarketDataKey, row: Row): MarketDataValueKey = {
    val fields = key.dataType.keyFields -- key.fieldValues().fields

    MarketDataValueKey(-1, row.filterKeys(fields.contains))
  }

  val failingXml= List("""
<starling.curves.SpreadStdDevSurfaceData>
 <periods>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>7</m></front><back class="starling.daterange.Month"><y>2011</y><m>8</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread/back"/><back class="starling.daterange.Month"><y>2011</y><m>9</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[2]/back"/><back class="starling.daterange.Month"><y>2011</y><m>10</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[3]/back"/><back class="starling.daterange.Month"><y>2011</y><m>11</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[4]/back"/><back class="starling.daterange.Month"><y>2011</y><m>12</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[5]/back"/><back class="starling.daterange.Month"><y>2012</y><m>1</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[5]/back"/><back class="starling.daterange.Month"><y>2012</y><m>12</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[6]/back"/><back class="starling.daterange.Month"><y>2012</y><m>2</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[8]/back"/><back class="starling.daterange.Month"><y>2012</y><m>3</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[9]/back"/><back class="starling.daterange.Month"><y>2012</y><m>4</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[10]/back"/><back class="starling.daterange.Month"><y>2012</y><m>5</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[11]/back"/><back class="starling.daterange.Month"><y>2012</y><m>6</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[12]/back"/><back class="starling.daterange.Month"><y>2012</y><m>7</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[13]/back"/><back class="starling.daterange.Month"><y>2012</y><m>8</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[14]/back"/><back class="starling.daterange.Month"><y>2012</y><m>9</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[15]/back"/><back class="starling.daterange.Month"><y>2012</y><m>10</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[16]/back"/><back class="starling.daterange.Month"><y>2012</y><m>11</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[17]/back"/><back class="starling.daterange.Month" reference="../../starling.daterange.Spread[7]/back"/></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[7]/back"/><back class="starling.daterange.Month"><y>2013</y><m>1</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[19]/back"/><back class="starling.daterange.Month"><y>2013</y><m>2</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[20]/back"/><back class="starling.daterange.Month"><y>2013</y><m>3</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[21]/back"/><back class="starling.daterange.Month"><y>2013</y><m>4</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[22]/back"/><back class="starling.daterange.Month"><y>2013</y><m>5</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[23]/back"/><back class="starling.daterange.Month"><y>2013</y><m>6</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[24]/back"/><back class="starling.daterange.Month"><y>2013</y><m>7</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[25]/back"/><back class="starling.daterange.Month"><y>2013</y><m>8</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[26]/back"/><back class="starling.daterange.Month"><y>2013</y><m>9</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[27]/back"/><back class="starling.daterange.Month"><y>2013</y><m>10</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[28]/back"/><back class="starling.daterange.Month"><y>2013</y><m>11</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[29]/back"/><back class="starling.daterange.Month"><y>2013</y><m>12</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.Spread[30]/back"/><back class="starling.daterange.Month"><y>2014</y><m>1</m></back></starling.daterange.SpreadPeriod>
 </periods>
 <atm>
  <double>1.6</double><double>1.55</double><double>1.45</double><double>1.55</double><double>1.5</double><double>1.5</double>
  <double>9.0</double><double>1.5</double><double>1.5</double><double>1.5</double><double>1.4</double><double>1.4</double>
  <double>1.4</double><double>1.3</double><double>1.3</double><double>1.2</double><double>1.2</double><double>1.1</double>
  <double>1.1</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double>
  <double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double>
  <double>1.0</double>
 </atm>
 <call>
  <double>0.3</double><double>0.3</double><double>0.3</double><double>0.2</double><double>0.2</double><double>0.2</double>
  <double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>-0.1</double><double>-0.1</double>
  <double>-0.1</double><double>-0.2</double><double>-0.2</double><double>-0.2</double><double>-0.2</double><double>-0.2</double>
  <double>-0.2</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double>
  <double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double>
  <double>0.0</double>
 </call>
 <put>
  <double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>0.9</double><double>0.9</double>
  <double>0.0</double><double>0.7</double><double>0.7</double><double>0.6</double><double>0.6</double><double>0.5</double>
  <double>0.5</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double>
  <double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double>
  <double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double>
  <double>0.4</double>
 </put>
 <uom>USD/bbl</uom>
</starling.curves.SpreadStdDevSurfaceData>
  """, """
<starling.curves.SpreadStdDevSurfaceData>
 <periods>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>2</m></front><back class="starling.daterange.Month"><y>2013</y><m>3</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>12</m></front><back class="starling.daterange.Month"><y>2012</y><m>1</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>4</m></front><back class="starling.daterange.Month"><y>2013</y><m>5</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>9</m></front><back class="starling.daterange.Month"><y>2011</y><m>10</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>1</m></front><back class="starling.daterange.Month"><y>2012</y><m>2</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>6</m></front><back class="starling.daterange.Month"><y>2012</y><m>7</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>3</m></front><back class="starling.daterange.Month"><y>2012</y><m>4</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>10</m></front><back class="starling.daterange.Month"><y>2011</y><m>11</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>8</m></front><back class="starling.daterange.Month"><y>2012</y><m>9</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>9</m></front><back class="starling.daterange.Month"><y>2012</y><m>10</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>8</m></front><back class="starling.daterange.Month"><y>2011</y><m>9</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>10</m></front><back class="starling.daterange.Month"><y>2012</y><m>11</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>12</m></front><back class="starling.daterange.Month"><y>2013</y><m>1</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>5</m></front><back class="starling.daterange.Month"><y>2013</y><m>6</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>7</m></front><back class="starling.daterange.Month"><y>2013</y><m>8</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>7</m></front><back class="starling.daterange.Month"><y>2012</y><m>8</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>5</m></front><back class="starling.daterange.Month"><y>2012</y><m>6</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>4</m></front><back class="starling.daterange.Month"><y>2012</y><m>5</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>6</m></front><back class="starling.daterange.Month"><y>2013</y><m>7</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>11</m></front><back class="starling.daterange.Month"><y>2011</y><m>12</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>1</m></front><back class="starling.daterange.Month"><y>2013</y><m>2</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>2</m></front><back class="starling.daterange.Month"><y>2012</y><m>3</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2013</y><m>3</m></front><back class="starling.daterange.Month"><y>2013</y><m>4</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2012</y><m>11</m></front><back class="starling.daterange.Month"><y>2012</y><m>12</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>12</m></front><back class="starling.daterange.Month"><y>2012</y><m>12</m></back><bitmap_-priv_-0>0</bitmap_-priv_-0></starling.daterange.SpreadPeriod>
 </periods>
 <atm>
  <double>1.0</double><double>1.4</double><double>1.0</double><double>1.3</double><double>1.4</double><double>1.3</double>
  <double>1.35</double><double>1.4</double><double>1.3</double><double>1.2</double><double>1.3</double><double>1.2</double>
  <double>1.1</double><double>1.0</double><double>1.0</double><double>1.3</double><double>1.3</double><double>1.3</double>
  <double>1.0</double><double>1.4</double><double>1.0</double><double>1.35</double><double>1.0</double><double>1.1</double>
  <double>9.0</double>
 </atm>
 <call>
  <double>0.0</double><double>0.1</double><double>0.0</double><double>0.3</double><double>-0.1</double><double>-0.4</double>
  <double>-0.3</double><double>0.1</double><double>-0.3</double><double>-0.2</double><double>0.3</double><double>-0.2</double>
  <double>-0.1</double><double>0.0</double><double>0.0</double><double>-0.3</double><double>-0.4</double><double>-0.3</double>
  <double>0.0</double><double>0.1</double><double>0.0</double><double>-0.2</double><double>0.0</double><double>-0.1</double>
  <double>0.0</double>
 </call>
 <put>
  <double>0.3</double><double>1.0</double><double>0.3</double><double>0.8</double>
  <double>0.9</double><double>0.6</double><double>0.8</double><double>1.0</double><double>0.4</double><double>0.4</double>
  <double>0.8</double><double>0.3</double><double>0.3</double><double>0.3</double><double>0.3</double><double>0.4</double>
  <double>0.6</double><double>0.8</double><double>0.3</double><double>1.0</double><double>0.3</double><double>0.9</double>
  <double>0.3</double><double>0.3</double><double>0.0</double>
 </put><uom>USD/bbl</uom>
</starling.curves.SpreadStdDevSurfaceData>
  """, """
<starling.marketdata.PriceFixingsHistoryData>
 <fixings class="map">
  <ordering class="starling.utils.conversions.Tuple2Ordering"/>
  <entry>
   <tuple>
    <starling.market.Level>Ask</starling.market.Level>
    <starling.daterange.StoredFixingPeriod>
     <period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>3</value><bitmap_-0>0</bitmap_-0></b></period>
     <bitmap_-priv_-0>0</bitmap_-priv_-0>
     <bitmap_-0>0</bitmap_-0>
    </starling.daterange.StoredFixingPeriod>
   </tuple>
   <starling.marketdata.MarketValue>
    <value class="scala.Left">
     <a class="starling.quantity.Quantity"><value>0.0</value><uom>USD/MT</uom><hashCode>0</hashCode></a>
    </value>
    <bitmap_-priv_-0>0</bitmap_-priv_-0>
    <bitmap_-0>0</bitmap_-0>
   </starling.marketdata.MarketValue>
  </entry>
  <entry>
   <tuple>
    <starling.market.Level>Bid</starling.market.Level>
    <starling.daterange.StoredFixingPeriod>
     <period class="scala.Right">
      <b class="starling.daterange.Tenor" reference="../../../../../entry/tuple/starling.daterange.StoredFixingPeriod/period/b"/>
     </period>
     <bitmap_-priv_-0>0</bitmap_-priv_-0>
     <bitmap_-0>0</bitmap_-0>
    </starling.daterange.StoredFixingPeriod>
   </tuple>
   <starling.marketdata.MarketValue>
    <value class="scala.Left">
     <a class="starling.quantity.Quantity">
      <value>0.0</value>
      <uom reference="../../../../../entry/starling.marketdata.MarketValue/value/a/uom"/>
      <hashCode>0</hashCode>
     </a>
    </value>
    <bitmap_-priv_-0>0</bitmap_-priv_-0>
    <bitmap_-0>0</bitmap_-0>
   </starling.marketdata.MarketValue>
  </entry>
 </fixings>
</starling.marketdata.PriceFixingsHistoryData>""", """
<starling.curves.SpreadStdDevSurfaceData>
 <periods>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month"><y>2011</y><m>7</m></front><back class="starling.daterange.Month"><y>2011</y><m>8</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod/back"/><back class="starling.daterange.Month"><y>2100</y><m>9</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[2]/back"/><back class="starling.daterange.Month"><y>2011</y><m>10</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[3]/back"/><back class="starling.daterange.Month"><y>2011</y><m>11</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[4]/back"/><back class="starling.daterange.Month"><y>2011</y><m>12</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[5]/back"/><back class="starling.daterange.Month"><y>2012</y><m>1</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[5]/back"/><back class="starling.daterange.Month"><y>2012</y><m>12</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[6]/back"/><back class="starling.daterange.Month"><y>2012</y><m>2</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[8]/back"/><back class="starling.daterange.Month"><y>2012</y><m>3</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[9]/back"/><back class="starling.daterange.Month"><y>2012</y><m>4</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[10]/back"/><back class="starling.daterange.Month"><y>2012</y><m>5</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[11]/back"/><back class="starling.daterange.Month"><y>2012</y><m>6</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[12]/back"/><back class="starling.daterange.Month"><y>2012</y><m>7</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[13]/back"/><back class="starling.daterange.Month"><y>2012</y><m>8</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[14]/back"/><back class="starling.daterange.Month"><y>2012</y><m>9</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[15]/back"/><back class="starling.daterange.Month"><y>2012</y><m>10</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[16]/back"/><back class="starling.daterange.Month"><y>2012</y><m>11</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[17]/back"/><back class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[7]/back"/></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[7]/back"/><back class="starling.daterange.Month"><y>2013</y><m>1</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[19]/back"/><back class="starling.daterange.Month"><y>2013</y><m>2</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[20]/back"/><back class="starling.daterange.Month"><y>2013</y><m>3</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[21]/back"/><back class="starling.daterange.Month"><y>2013</y><m>4</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[22]/back"/><back class="starling.daterange.Month"><y>2013</y><m>5</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[23]/back"/><back class="starling.daterange.Month"><y>2013</y><m>6</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[24]/back"/><back class="starling.daterange.Month"><y>2013</y><m>7</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[25]/back"/><back class="starling.daterange.Month"><y>2013</y><m>8</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[26]/back"/><back class="starling.daterange.Month"><y>2013</y><m>9</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[27]/back"/><back class="starling.daterange.Month"><y>2013</y><m>10</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[28]/back"/><back class="starling.daterange.Month"><y>2013</y><m>11</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[29]/back"/><back class="starling.daterange.Month"><y>2013</y><m>12</m></back></starling.daterange.SpreadPeriod>
  <starling.daterange.SpreadPeriod><front class="starling.daterange.Month" reference="../../starling.daterange.SpreadPeriod[30]/back"/><back class="starling.daterange.Month"><y>2014</y><m>1</m></back></starling.daterange.SpreadPeriod>
 </periods>
 <atm>
  <double>1.6</double><double>1.55</double><double>1.45</double><double>1.55</double><double>1.5</double><double>1.5</double>
  <double>9.0</double><double>1.5</double><double>1.5</double><double>1.5</double><double>1.4</double><double>1.4</double>
  <double>1.4</double><double>1.3</double><double>1.3</double><double>1.2</double><double>1.2</double><double>1.1</double>
  <double>1.1</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double>
  <double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double>
  <double>1.0</double>
 </atm>
 <call>
  <double>0.3</double><double>0.3</double><double>0.3</double><double>0.2</double><double>0.2</double><double>0.2</double>
  <double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>-0.1</double><double>-0.1</double>
  <double>-0.1</double><double>-0.2</double><double>-0.2</double><double>-0.2</double><double>-0.2</double><double>-0.2</double>
  <double>-0.2</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double>
  <double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double><double>0.0</double>
  <double>0.0</double>
 </call>
 <put>
  <double>1.0</double><double>1.0</double><double>1.0</double><double>1.0</double><double>0.9</double><double>0.9</double>
  <double>0.0</double><double>0.7</double><double>0.7</double><double>0.6</double><double>0.6</double><double>0.5</double>
  <double>0.5</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double>
  <double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double>
  <double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double><double>0.4</double>
  <double>0.1234</double>
 </put>
 <uom>USD/bbl</uom>
</starling.curves.SpreadStdDevSurfaceData>""", """
<starling.marketdata.PriceFixingsHistoryData>
 <fixings class="map">
  <ordering class="starling.utils.conversions.Tuple2Ordering"/>
  <entry>
   <tuple>
    <starling.market.Level>Close</starling.market.Level><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>1</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.01951</value></b></value></starling.marketdata.MarketValue></entry><entry><tuple><starling.market.Level reference="../../../entry/tuple/starling.market.Level"/><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>2</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.023133</value></b></value></starling.marketdata.MarketValue></entry><entry><tuple><starling.market.Level reference="../../../entry/tuple/starling.market.Level"/><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>3</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.02604</value></b></value></starling.marketdata.MarketValue></entry><entry><tuple><starling.market.Level reference="../../../entry/tuple/starling.market.Level"/><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>4</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.028065000000000003</value></b></value></starling.marketdata.MarketValue></entry><entry><tuple><starling.market.Level reference="../../../entry/tuple/starling.market.Level"/><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>5</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.029815</value></b></value></starling.marketdata.MarketValue></entry><entry><tuple><starling.market.Level reference="../../../entry/tuple/starling.market.Level"/><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>10</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.035065</value></b></value></starling.marketdata.MarketValue></entry><entry><tuple><starling.market.Level reference="../../../entry/tuple/starling.market.Level"/><starling.daterange.StoredFixingPeriod><period class="scala.Right"><b class="starling.daterange.Tenor"><tenorName>M</tenorName><value>20</value></b></period></starling.daterange.StoredFixingPeriod></tuple><starling.marketdata.MarketValue><value class="scala.Right"><b class="starling.quantity.Percentage"><value>0.038755000000000005</value></b></value></starling.marketdata.MarketValue>
  </entry>
 </fixings>
 </starling.marketdata.PriceFixingsHistoryData>
""")

  def cleanUpXml(xml: String): String = if (xml.contains("Spread")) {
    xml.replaceAll("""starling\.daterange\.SpreadPeriodPeriod""", """starling.daterange.SpreadPeriod""")
       .replaceAll("""\.\.\/starling\.daterange\.Spread\/""", """\.\.\/starling\.daterange\.SpreadPeriod/""")
       .replaceAll("""\.\.\/starling\.daterange\.Spread\[""", """\.\.\/starling\.daterange\.SpreadPeriod[""")
       .replaceAll("""\<bitmap_-priv_-0\>.*\<\/bitmap_-priv_-0\>""", "")
  } else {
    xml
  }

  def testReadingOfCleanedUpXml {
    val period = SpreadPeriod(Day.yesterday, Day.today)
    val periods: Array[Period] = Array(SpreadPeriod(Day.today, Day.today + 1), period, period)

    val surfaceData = SpreadStdDevSurfaceData(periods, Array(1.1, 2.2, 3.3), Array(1.1, 2.2, 3.3), Array(1.1, 2.2, 3.3), UOM.BRL)

    val xml = StarlingXStream.write(surfaceData)

    log.debug(xml)

    failingXml.foreach(xml => {
      val obj = StarlingXStream.read(cleanUpXml(xml))

      obj.safeCast[PriceFixingsHistoryData].map { priceFixingsHistoryData => {
        val rows: Iterable[Row] = PriceFixingsHistoryDataType.castRows(
          PriceFixingsHistoryDataKey("", Some("")), priceFixingsHistoryData, ReferenceDataLookup.Null)

        println(rows)
      } }

      log.debug(obj.toString)
    })
  }
}