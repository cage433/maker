package starling.services

import starling.db._
import starling.props.PropsHelper
import starling.utils._
import sql.PersistAsBlob
import starling.utils.ImplicitConversions._
import java.lang.String
import starling.daterange.ObservationTimeOfDay
import starling.pivot.{Field, PivotQuantity}
import collection.SortedMap
import starling.quantity.{Quantity, Percentage}
import collection.immutable.{TreeMap, Map}
import starling.marketdata.{MarketDataType, MarketDataKey}
import starling.instrument.utils.StarlingXStream

case class FirstKey(time: ObservationTimeOfDay, marketDataSet: MarketDataSet, marketDataType: MarketDataType, key: MarketDataKey)

case class SecondKey(key: SortedMap[Field, Any])

object Bob {

  private val tableNameMarketDataCommit = "MarketDataCommit"
  private val tableNameMarketDataExtendedKey = "MarketDataExtendedKey"
  private val tableNameMarketDataValue = "MarketDataValue"
  private val tableNameMarketDataValueKey = "MarketDataValueKey"
  private val tableNameMarketDataTag = "MarketDataTag"

  private val alterTableMarketDataTagSql = "alter table [dbo].%s add commitId int".format(tableNameMarketDataTag)

  private val createTableMarketDataCommitSql = """
  create table [dbo].""" + tableNameMarketDataCommit + """ (
    id            int IDENTITY(1,1) NOT NULL
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
    , value text
  )
  """

  private val createTableMarketDataValueSql = """
  create table [dbo].""" + tableNameMarketDataValue + """ (
    observationDay  date
    , extendedKey   int
    , valueKey      int
    , value         decimal(9)
    , uom           varchar(12)
    , commitId      int
  )
  """

  def main(args: Array[String]) {

    val skipCount = 200

    val props = PropsHelper.defaultProps

    val init = new StarlingInit(props)
    Log.infoWithTime("T") {
      init.starlingDB.inTransaction(writer => {

        println("[debug] Dropping tables...")
        for (tableName <- List(tableNameMarketDataValue, tableNameMarketDataCommit, tableNameMarketDataExtendedKey, tableNameMarketDataValueKey)) {
          writer.update("drop table " + tableName)
        }

        println("[debug] Creating tables...")
        // writer.update(alterTableMarketDataTagSql)
        writer.update("update " + tableNameMarketDataTag + " set commitId=null")
        writer.update(createTableMarketDataExtendedKeySql)
        writer.update(createTableMarketDataValueKeySql)
        writer.update(createTableMarketDataValueSql)
        writer.update(createTableMarketDataCommitSql)

        val mainKeyMapper = new scala.collection.mutable.HashMap[FirstKey, Long]()
        def idForMainKey(key: FirstKey) = mainKeyMapper.getOrElseUpdate(key, {
          val params = Map("marketDataSet" -> key.marketDataSet.name, "marketDataType" -> key.marketDataType.name, "observationTime" -> key.time.name, "marketDataKey" -> new PersistAsBlob(key.key))
          val id = writer.insertAndReturnKey(tableNameMarketDataExtendedKey, "id", params)
          id
        })

        val valueKeyMapper = new scala.collection.mutable.HashMap[SecondKey, Long]()
        def idForValueKey(key: SecondKey) = valueKeyMapper.getOrElseUpdate(key, {
          val params = Map("value" -> new PersistAsBlob(key.key))
          val id = writer.insertAndReturnKey("MarketDataValueKey", "id", params)
          id
        })

        println("[debug] Mapping MarketDataValue...")
        var counter = 0
        var buffer = new scala.collection.mutable.ArrayBuffer[Map[String, Any]]()
        init.starlingDB.query("select * from MarketData") {
          rs => {

            counter += 1
            if ((counter % 100) == 0) println(counter)

            val observationDay = rs.getDayOption("observationDay")
            val time = rs.getString("observationTime")
            val marketDataSet = rs.getString("marketDataSet")
            val marketDataType = rs.getObject[MarketDataType]("marketDataType")
            val key = rs.getObject[MarketDataKey]("marketDataKey")
            val version = rs.getInt("version")
            val timestamp = rs.getTimestamp("timestamp")
            val commitId = writer.insertAndReturnKey("MarketDataCommit", "id", Map("timestamp" -> timestamp))

            val rows = writer.update("UPDATE [dbo]." + tableNameMarketDataTag + " SET commitId=" + commitId + " WHERE version=" + version)
            if (rows > 0) printf("Updated %d row(s) for commitId: %d to version: %d\n", rows, commitId, version)

            val firstKey = FirstKey(ObservationTimeOfDay.fromName(time), MarketDataSet(marketDataSet), marketDataType, key)

            // special cases for xstream string data that has been only partially refactored previously...
            val data = rs.getStringOption("data");
            val refactoredData = data.map(xml => {
              StarlingXStream.read(xml.replaceAll("""starling\.daterange\.SpreadPeriodPeriod""", """starling.daterange.SpreadPeriod"""))
            })
            // map the refactored data...
            refactoredData.map(md => key.castRows(key.unmarshallDB(md))) match {
              case None => {
                //delete
              }
              case Some(rows) => {
                rows.foreach {
                  row => {
                    val secondKey = {
                      val fieldsForSecondKey = key.dataType.keyFields -- key.fieldValues.keySet
                      SecondKey(TreeMap.empty[Field, Any](Field.ordering) ++ (row.filterKeys(fieldsForSecondKey.contains)))
                    }
                    val aaa: List[Any] = key.dataType.valueFields.toList.flatMap(f => row.get(f))
                    val uomValueOption: Option[(String, Double)] = aaa match {
                      case Nil => println("[debug] Nil " + key); None
                      case one :: Nil => {
                        one match {
                          case q: Quantity => Some((q.uom.toString, q.value))
                          case pq: PivotQuantity if pq.quantityValue.isDefined => Some((pq.quantityValue.get.uom.toString, pq.quantityValue.get.value))
                          case pc: Percentage => Some(("%", pc.value))
                          case other => println("[debug] unexpected value " + other.asInstanceOf[AnyRef].getClass + " " + other); None
                        }
                      }
                      case many => println("[debug] Many " + key + " " + many); None
                    }
                    uomValueOption match {
                      case Some((uom, value)) => {
                        val mainKey = idForMainKey(firstKey)
                        val valueKey = idForValueKey(secondKey)
                        val params = Map(
                          "observationDay" -> observationDay.getOrElse(null), "extendedKey" -> mainKey, "valueKey" -> valueKey,
                          "value" -> value, "uom" -> uom, "commitId" -> commitId
                        )
                        buffer.append(params)
                        if (buffer.size > 2000) {
                          writer.insert(tableNameMarketDataValue, buffer.toList)
                          buffer.clear
                        }
                      }
                      case None => //skip
                    }
                  }
                }
              }
            }
          }
        }
        if (buffer.nonEmpty) {
          writer.insert(tableNameMarketDataValue, buffer.toList)
        }

        // update the tag version to the new commit Id
        init.starlingDB.queryWithOneResult("select count(*) as nullRowCount from " + tableNameMarketDataTag + " where commitId is null") {
          rs => {
            val missingCommitIdCount = rs.getLong("nullRowCount")
            println(missingCommitIdCount + " MarketDataTag row(s) have no commit Id")
            //Assert.assertEquals( rs.getLong("nullRowCount"), 0, "Migration error: some tagged versions have no commit Id")
          }
        }
      })
    }
  }
}
