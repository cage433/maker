package starling.trade.internal.osgi

import starling.tradestore.TradeStores
import starling.trade.internal.TradeServiceImpl
import starling.trade.TradeService
import starling.services.excel.TradeHandler
import starling.services.trade.ExcelTradeReader
import starling.utils.Broadcaster
import starling.tradestore.intraday.IntradayTradeStore
import starling.gui.api.{EAIDeskInfo, Desk}
import starling.tradestore.eai.EAITradeStore
import starling.neptune.{RefinedFixationSystemOfRecord, RefinedFixationTradeStore, RefinedAssignmentSystemOfRecord, RefinedAssignmentTradeStore}
import starling.tradeimport.{ClosedDesks, TradeImporterFactory, TradeImporter}
import starling.titan.{TitanSystemOfRecord, TitanTradeStore}
import starling.instrument.TradeSystem
import starling.db.{RefinedFixationTradeSystem, RefinedAssignmentTradeSystem, TitanTradeSystem, DB}
import starling.eai.{EAIAutoImport, EAIDealBookMapping, EAIStrategyDB, Traders}
import starling.richdb.{RichDB, RichResultSetRowFactory}
import starling.auth.{User, LdapUserLookup}
import starling.manager._
import starling.rmi.StarlingServer

class TradeBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {

    val broadcaster = context.awaitService(classOf[Broadcaster])
    val props = context.awaitService(classOf[starling.props.Props])
    val ldapUserLookup = context.awaitService(classOf[LdapUserLookup])

    context.awaitService(classOf[StarlingServer]) //Hack to ensure that a MarketProvider has been set

    val starlingRichDB = new RichDB(props.StarlingDatabase(), new RichResultSetRowFactory)
    val starlingDB = starlingRichDB.db

    val eaiSqlServerDB = DB(props.EAIReplica())
    val strategyDB = new EAIStrategyDB(eaiSqlServerDB)
    val eaiTradeStores = Desk.eaiDesks.map{case desk@Desk(_, _, Some(_:EAIDeskInfo)) => desk ->
      new EAITradeStore(starlingRichDB, broadcaster, strategyDB, desk)}.toMap
    val intradayTradesDB = new IntradayTradeStore(starlingRichDB, strategyDB, broadcaster, ldapUserLookup)

    val eaiStarlingDB = DB(props.EAIStarlingSqlServer())
    val eaiDealBookMapping = new EAIDealBookMapping(eaiSqlServerDB)

    val titanTradeStore = new TitanTradeStore(starlingRichDB, broadcaster, TitanTradeSystem)

    val enabledDesks: Set[Desk] = props.EnabledDesks().trim.toLowerCase match {
      case "" => throw new Exception("EnabledDesks property not set, valid values: all, none, " + Desk.names.mkString(", "))
      case "all" => Desk.values.toSet
      case "none" => Set.empty
      case names => names.split(",").toList.map(Desk.fromName).toSet
    }

    val closedDesks = new ClosedDesks(broadcaster, starlingDB)

    val tradeStores = new TradeStores(
      closedDesks,
      eaiTradeStores, intradayTradesDB,
      titanTradeStore)

    context.createServiceTracker(Some(classOf[TradeImporter]), Nil, new BromptonServiceCallback[TradeImporter] {
      def serviceAdded(ref: BromptonServiceReference, importer: TradeImporter) {
        tradeStores.registerTradeImporter(ref, importer)
      }
      def serviceRemoved(ref: BromptonServiceReference) {
        tradeStores.unregister(ref)
      }
    })

    val eaiStarlingRichSqlServerDB = new RichDB(props.EAIStarlingSqlServer(), new RichResultSetRowFactory)
    val eaiAutoImport = new EAIAutoImport(15, starlingRichDB, eaiStarlingRichSqlServerDB, strategyDB, eaiTradeStores, closedDesks, enabledDesks)


    val allTraders = new Traders(ldapUserLookup.user _)

    val tradeService = new TradeServiceImpl(tradeStores, enabledDesks, allTraders, eaiStarlingDB, props.Production())

    val tradeHandler = new TradeHandler(
      broadcaster,
      new ExcelTradeReader(strategyDB, eaiDealBookMapping, allTraders, User.currentlyLoggedOn _),
      tradeStores.intradayTradeStore, allTraders
    )

    context.registerService(classOf[TradeStores], tradeStores)
    context.registerService(classOf[AnyRef], tradeHandler, ExportExcelProperty::Nil)
    context.registerService(classOf[TradeService], tradeService, ExportGuiRMIProperty::Nil)
  }

  def stop(context: BromptonContext) {}
}