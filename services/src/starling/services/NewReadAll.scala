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
import collection.SortedMap
import starling.quantity.{UOM, Quantity, Percentage}
import collection.immutable.{TreeMap, Map}
import starling.titan.{TitanSystemOfRecord, TitanTradeStore}
import com.trafigura.services.ResteasyServiceApi._
import com.trafigura.services.trinity.TrinityService
import com.trafigura.services.ResteasyServiceApi
import starling.marketdata.{MarketDataTypes, MarketDataKey}


object NewReadAll {
  def main(args: Array[String]) {
    val props = PropsHelper.defaultProps

    val init = new StarlingInit(props)


    Log.infoWithTime("values") {
        init.starlingDB.query("select observationDay, extendedKey, valueKey, value, commitid from MarketDataValues") { rs => {
          val day = rs.getDayOption("observationDay")
          val firstKey = rs.getInt("extendedKey")
          val secondKey = rs.getInt("valueKey")
          val value = rs.getDouble("value")
  //        val uom = {
  //          val text = rs.getString("uom")
  //          if (text == "") UOM.NULL else UOM.fromString(text)
  //        }
          val timestamp = rs.getInt("commitid")
        }}
    }

    System.exit(0)


    Log.infoWithTime("Readall") {
      val firstKeys = Log.infoWithTime("firstkeys") { Map() ++ init.starlingDB.queryWithResult("select * from ExtendedMarketDataKey") { rs => {
        val id = rs.getInt("id")
        val time = ObservationTimeOfDay.fromName(rs.getString("observationTime"))
        val marketDataSet = MarketDataSet(rs.getString("marketDataSet"))
        val marketDataType = MarketDataTypes.fromName(rs.getString("marketDataSet"))
        val key = rs.getObject[MarketDataKey]("marketDataKey")
        id -> FirstKey(time, marketDataSet, marketDataType, key)
      }} }
      val secondKeys = Log.infoWithTime("valueKeys") { Map() ++ init.starlingDB.queryWithResult("select * from ValueKey") { rs => {
        rs.getInt("id") -> SecondKey(rs.getObject[SortedMap[Field,Any]]("value"))
      }} }
      val commits = Log.infoWithTime("commits") { Map() ++ init.starlingDB.queryWithResult("select * from MarketDataCommit") { rs => {
        rs.getInt("id") -> rs.getTimestamp("timestamp")
      }} }
      Log.infoWithTime("values") {
        init.starlingDB.query("select * from MarketDataValues") { rs => {
          val day = rs.getDayOption("observationDay")
          val firstKey = firstKeys(rs.getInt("extendedKey"))
          val secondKey = secondKeys(rs.getInt("valueKey"))
          val value = rs.getDouble("value")
          val uom = {
            val text = rs.getString("uom")
            if (text == "") UOM.NULL else UOM.fromString(text)
          }
          val timestamp = commits(rs.getInt("commitid"))
        }}
      }
    }
  }
}




/**
 * The main entry point into Starling
 */











