package starling.services

import excel._
import jmx.StarlingJMX
import rpc.logistics.DefaultTitanLogisticsServices
import rpc.marketdata.MarketDataService
import rpc.valuation._
import starling.schemaevolution.system.PatchRunner
import starling.db._
import starling.richdb.{RichDB, RichResultSetRowFactory}
import starling.market._
import starling.props.{PropsHelper, Props}
import starling.tradestore.eai.EAITradeStore
import java.net.InetAddress
import starling.tradestore.intraday.IntradayTradeStore
import starling.neptune.{RefinedFixationSystemOfRecord, RefinedFixationTradeStore, RefinedAssignmentSystemOfRecord, RefinedAssignmentTradeStore}
import starling.curves.readers._
import trade.ExcelTradeReader
import trinity.{TrinityUploader, XRTGenerator, TrinityUploadCodeMapper, FCLGenerator}
import java.io.File
import starling.auth.{LdapUserLookup, User, ServerLogin}
import java.util.concurrent.CopyOnWriteArraySet
import starling.utils._
import sql.{PersistAsBlob, ConnectionParams}
import starling.utils.ImplicitConversions._
import starling.tradeimport.{ClosedDesks, TradeImporterFactory, TradeImporter}
import starling.tradestore.TradeStores
import starling.http._
import starling.trade.TradeSystem
import starling.reports.pivot.{ReportContextBuilder, ReportService}
import starling.LIMServer
import starling.gui.api._
import starling.bouncyrmi._
import starling.eai.{Book, Traders, EAIAutoImport, EAIStrategyDB}
import org.springframework.mail.javamail.JavaMailSenderImpl
import starling.rmi._
import starling.calendar._
import java.lang.String
import com.trafigura.services.valuation.{ValuationServiceApi, TradeManagementCacheNotReady}
import org.jboss.netty.channel.{ChannelLocal, Channel}
import com.trafigura.services.marketdata.MarketDataServiceApi
import starling.curves.{StarlingMarketLookup, FwdCurveAutoImport, CurveViewer}
import starling.services.rpc.refdata._
import starling.services.rabbit._
import starling.daterange.ObservationTimeOfDay
import starling.pivot.{Field, PivotQuantity}
import starling.marketdata.{MarketDataKey}
import collection.SortedMap
import starling.quantity.{UOM, Quantity, Percentage}
import collection.immutable.{TreeMap, Map}
import starling.titan.{TitanSystemOfRecord, TitanTradeStore}
import com.trafigura.services.ResteasyServiceApi._
import com.trafigura.services.trinity.TrinityService
import com.trafigura.services.ResteasyServiceApi


object Bob {
  def main(args: Array[String]) {

    val props = PropsHelper.defaultProps

    val init = new StarlingInit(props)
    Log.infoWithTime("T") {
      init.starlingDB.inTransaction( writer => {
        writer.update("drop table ExtendedMarketDataKey")
        writer.update("drop table ValueKey")
        writer.update("drop table MarketDataValues")
        writer.update("drop table MarketDataCommit")
        writer.update("create table [dbo].ExtendedMarketDataKey (id int IDENTITY(1,1) NOT NULL, marketDataSet varchar(255), observationTime varchar(255), marketDataKey text)")
        writer.update("create table [dbo].ValueKey (id int IDENTITY(1,1) NOT NULL, value text)")
        writer.update("create table [dbo].MarketDataValues (observationDay datetime, extendedKey int, valueKey int, value decimal(9), uom varchar(12), commitid int)")
        writer.update("create table [dbo].MarketDataCommit (id int IDENTITY(1,1) NOT NULL, timestamp datetime, username varchar(128))")

        val mainKeyMapper = new scala.collection.mutable.HashMap[FirstKey,Long]()
        def idForMainKey(key:FirstKey) = mainKeyMapper.getOrElseUpdate(key, {
          val params = Map("marketDataSet" -> key.marketDataSet.name, "observationTime" -> key.time.name, "marketDataKey" -> new PersistAsBlob(key.key))
          writer.insertAndReturnKey("ExtendedMarketDataKey", "id", params)
        })
        val valueKeyMapper = new scala.collection.mutable.HashMap[SecondKey,Long]()
        def idForValueKey(key:SecondKey) = valueKeyMapper.getOrElseUpdate(key, {
          val params = Map("value" -> new PersistAsBlob(key.key))
          writer.insertAndReturnKey("ValueKey", "id", params)
        })
        var counter = 0
        var buffer = new scala.collection.mutable.ArrayBuffer[Map[String,Any]]()
        init.starlingDB.query("select * from MarketData where version % 2 = 0") { rs => {

          counter += 1
          if ((counter % 100) == 0) println(counter)

          val observationDay = rs.getDayOption("observationDay")
          val time = rs.getString("observationTime")
          val marketDataSet = rs.getString("marketDataSet")
          //val marketDataType = rs.getObject[MarketDataType]("marketDataType")
          val key = rs.getObject[MarketDataKey]("marketDataKey")
          val version = rs.getInt("version")
          val timestamp = rs.getTimestamp("timestamp")
          val commit = writer.insertAndReturnKey("MarketDataCommit", "id", Map("timestamp" -> timestamp))
          val firstKey = FirstKey(ObservationTimeOfDay.fromName(time), MarketDataSet(marketDataSet), key)
          rs.getObjectOption[Any]("data").map(md => key.castRows(key.unmarshallDB(md))) match {
            case None => { //delete

            }
            case Some(rows) => {
              rows.foreach { row => {
                val secondKey = {
                  val fieldsForSecondKey = key.dataType.keyFields -- key.fieldValues.keySet
                  SecondKey(TreeMap.empty[Field,Any](Field.ordering) ++ (row.filterKeys(fieldsForSecondKey.contains)))
                }
                val aaa: List[Any] = key.dataType.valueFields.toList.flatMap(f=>row.get(f))
                val uomValueOption: Option[(String,Double)] = aaa match {
                  case Nil => println("Nil " + key); None
                  case one :: Nil => {
                    one match {
                      case q:Quantity => Some( (q.uom.toString, q.value) )
                      case pq:PivotQuantity if pq.quantityValue.isDefined => Some( (pq.quantityValue.get.uom.toString, pq.quantityValue.get.value) )
                      case pc:Percentage => Some( ("%", pc.value) )
                      case other => println("unexpected value " + other.asInstanceOf[AnyRef].getClass + " " + other); None
                    }
                  }
                  case many => println("Many " + key + " " + many); None
                }
                uomValueOption match {
                  case Some((uom, value)) => {
                    val mainKey = idForMainKey(firstKey)
                    val valueKey = idForValueKey(secondKey)
                    val params = Map(
                      "observationDay" -> observationDay.getOrElse(null), "extendedKey" -> mainKey, "valueKey" -> valueKey,
                      "value" -> value, "uom" -> uom, "commitid" -> commit
                    )
                    buffer.append(params)
                    if (buffer.size > 2000) {
                      writer.insert("MarketDataValues", buffer.toList)
                      buffer.clear
                    }
                  }
                  case None => //skip
                }
              } }
            }
          }
        }}
        if (buffer.nonEmpty) {
          writer.insert("MarketDataValues", buffer.toList)
        }
      })
    }







//    db.inTransaction( writer => {
//      writer.update("""
//      CREATE TABLE [dbo].[MarketDataTagXX](
//	[snapshotid] [int] NOT NULL identity (10000,1) PRIMARY KEY,
//	[version] [int] NOT NULL,
//	[observationDay] [datetime] NOT NULL,
//	[pricingGroup] [varchar](512) NOT NULL,
//	[timestamp] [datetime] NOT NULL
//) ON [PRIMARY] """)
//      val rows = db.queryWithResult("select * from MarketDataTag", Map()) { rs => rs.asMap }
//      println(writer.update("SET IDENTITY_INSERT MarketDataTagXX1 ON"))
//      println(writer.update("SET IDENTITY_INSERT MarketDataTagXX OFF"))
//      println(writer.update("SET IDENTITY_INSERT MarketDataTagXX ON"))
//      writer.insert("MarketDataTagXX", rows)
//    })
//  }


//    db.inTransaction( writer => {
//      writer.update("alter table MarketDataTagX add [version] [int] ")
//    	//writer.update("alter table MarketDataTag2 add [observationDay] [datetime] NOT NULL")
//	    writer.update("alter table MarketDataTagX add [pricingGroup] [varchar](512) ")
//	    writer.update("alter table MarketDataTagX add [timestamp] [datetime] ")
//
//      import starling.utils.sql.QueryBuilder._
//
//      db.query("select * from MarketDataTag", Map()) {
//        rs => writer.update("MarketDataTagX", Map("version"→rs.getInt("version"), "pricingGroup"→rs.getString("pricingGroup"), "timestamp"→rs.getTimestamp("timestamp")), ("snapshotid") eql rs.getInt("snapshotid"))
//      }
//
//      //writer.update("alter table MarketDataTagX drop column revalGroup")
//      //throw new Exception("Aborted Stop")
//    })
//  }

    //  db.inTransaction( writer => {
//    writer.update("CREATE TABLE [dbo].[FooXX]( [snapshotid] [int] NOT NULL identity (10000,1) PRIMARY KEY ) ON [PRIMARY]")
//    println(writer.update("SET IDENTITY_INSERT FooXX ON"))
//    writer.insert("FooXX", Map("snapshotid"->8))
//    throw new Exception("Stop")
//  })
  }
}

case class FirstKey(time:ObservationTimeOfDay, marketDataSet:MarketDataSet, key:MarketDataKey)
case class SecondKey(key:SortedMap[Field,Any])