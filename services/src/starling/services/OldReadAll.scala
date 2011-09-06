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
import starling.dbx.ConnectionParams
import sql.{PersistAsBlob}
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


object OldReadAll {
  def main(args: Array[String]) {
    val props = PropsHelper.defaultProps

    val init = new StarlingInit(props)

    Log.infoWithTime("Readall") {
      init.starlingDB.query("select * from MarketData where version % 2 = 0") { rs => {
        val observationDay = rs.getDayOption("observationDay")
        val time = rs.getString("observationTime")
        val marketDataSet = rs.getString("marketDataSet")
        //val marketDataType = rs.getObject[MarketDataType]("marketDataType")
        val key = rs.getObject[MarketDataKey]("marketDataKey")
        val version = rs.getInt("version")
        val timestamp = rs.getTimestamp("timestamp")
        val marketData = rs.getObjectOption[Any]("data")
      } }
    }
  }
}




/**
 * The main entry point into Starling
 */











