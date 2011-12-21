package starling.trade.impl.osgi

import starling.tradestore.TradeStores
import starling.trade.impl.TradeFacilityImpl
import starling.services.excel.TradeHandler
import starling.services.trade.ExcelTradeReader
import starling.manager.Broadcaster
import starling.tradestore.intraday.IntradayTradeStore
import starling.tradestore.eai.EAITradeStore
import starling.neptune.{RefinedFixationSystemOfRecord, RefinedFixationTradeStore, RefinedAssignmentSystemOfRecord, RefinedAssignmentTradeStore}
import starling.tradeimport.{ClosedDesks, TradeImporterFactory, TradeImporter}
import starling.titan.{TitanSystemOfRecord, TitanTradeStore}
import starling.instrument.TradeSystem
import starling.db.{RefinedFixationTradeSystem, RefinedAssignmentTradeSystem, TitanTradeSystem, DB}
import starling.eai.{EAIAutoImport, EAIDealBookMapping, EAIStrategyDB}
import starling.richdb.{RichDB, RichResultSetRowFactory}
import starling.auth.{User, LdapUserLookup}
import starling.manager._
import starling.rmi.StarlingServer
import starling.trade.facility.TradeFacility
import starling.marketdata.DBReferenceDataLookup
import starling.props.ServerTypeLabel
import starling.gui.api.{EnabledDesks, EAIDeskInfo, Desk}

class TradeBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {

    val broadcaster = context.awaitService(classOf[Broadcaster])
    val props = context.awaitService(classOf[starling.props.Props])
    val ldapUserLookup = context.awaitService(classOf[LdapUserLookup])

    val enabledDesks: Set[Desk] = props.EnabledDesks().trim.toLowerCase match {
      case "" => props.ServerType() match {
        case ServerTypeLabel.FC2 => Set(Desk.Titan)
        case ServerTypeLabel.Oil => Desk.oilDesks
        case ServerTypeLabel.Dev => Desk.values.toSet
        case other => throw new Exception("Unknown ServerType: " + other)
      }
      case "all" => Desk.values.toSet
      case "none" => Set.empty
      case names => names.split(",").toList.map(Desk.fromName).toSet
    }
    context.registerService(classOf[EnabledDesks], EnabledDesks(enabledDesks))

    context.awaitService(classOf[StarlingServer]) //Hack to ensure that a MarketProvider has been set

    val starlingRichDB = new RichDB(props.StarlingDatabase(), new RichResultSetRowFactory)
    val starlingDB = starlingRichDB.db

    val neptuneRichDB = new RichDB(props.NeptuneDatabase(), new RichResultSetRowFactory)
    val closedDesks = new ClosedDesks(broadcaster, starlingDB)

    val eaiSqlServerDB = DB(props.EAIReplica())
    val strategyDB = new EAIStrategyDB(eaiSqlServerDB)
    val eaiTradeStores = Desk.eaiDesks.map{case desk@Desk(_, _, Some(_:EAIDeskInfo)) => desk ->
      new EAITradeStore(starlingRichDB, broadcaster, strategyDB, desk, closedDesks)}.toMap
    val intradayTradesDB = new IntradayTradeStore(starlingRichDB, strategyDB, broadcaster, ldapUserLookup, closedDesks)

    val eaiStarlingDB = DB(props.EAIStarlingSqlServer())
    val eaiDealBookMapping = new EAIDealBookMapping(eaiSqlServerDB)

    val neptuneReferenceDataLookup = DBReferenceDataLookup(neptuneRichDB)
    val titanTradeStore = new TitanTradeStore(starlingRichDB, broadcaster, TitanTradeSystem, neptuneReferenceDataLookup, closedDesks)

    val tradeStores = new TradeStores(
      closedDesks,
      eaiTradeStores, intradayTradesDB,
      titanTradeStore,
      enabledDesks
    )

    context.createServiceTracker(Some(classOf[TradeImporter]), ServiceProperties(), new BromptonServiceCallback[TradeImporter] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, importer: TradeImporter) {
        tradeStores.registerTradeImporter(ref, importer)
      }
      override def serviceRemoved(ref: BromptonServiceReference) {
        tradeStores.unregister(ref)
      }
    })

    val eaiStarlingRichSqlServerDB = new RichDB(props.EAIStarlingSqlServer(), new RichResultSetRowFactory)
    val eaiAutoImport = new EAIAutoImport(15, starlingRichDB, eaiStarlingRichSqlServerDB, strategyDB, eaiTradeStores, closedDesks, enabledDesks)
    if (props.ImportsBookClosesFromEAI())
      eaiAutoImport.start // TODO Thomas -- move this somewhere sensible

    val tradeService = new TradeFacilityImpl(tradeStores, eaiStarlingDB, props.Production())

    val tradeHandler = new TradeHandler(
      broadcaster,
      new ExcelTradeReader(strategyDB, eaiDealBookMapping, User.currentlyLoggedOn _),
      tradeStores.intradayTradeStore
    )

    context.registerService(classOf[TradeStores], tradeStores)
    context.registerService(classOf[AnyRef], tradeHandler, ServiceProperties(ExportXlloopProperty, ExportLoopyProperty))
    context.registerService(classOf[TradeFacility], tradeService, ServiceProperties(ExportGuiRMIProperty))
  }
}
