package starling.services

import excel._
import jmx.StarlingJMX
import rpc.logistics.{FileMockedTitanLogisticsServices, DefaultTitanLogisticsServices}
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
import trinity.{TrinityUploader, XRTGenerator, FCLGenerator}
import starling.dbx.ConnectionParams
import starling.utils.ImplicitConversions._
import starling.tradeimport.{ClosedDesks, TradeImporterFactory, TradeImporter}
import starling.tradestore.TradeStores
import starling.http._
import starling.instrument.TradeSystem
import starling.LIMServer
import starling.gui.api._
import starling.eai.{Traders, EAIAutoImport, EAIStrategyDB}
import org.springframework.mail.javamail.JavaMailSenderImpl
import starling.rmi._
import starling.calendar._
import com.trafigura.services.valuation.{ValuationServiceApi, TradeManagementCacheNotReady}
import starling.curves.{StarlingMarketLookup, FwdCurveAutoImport, CurveViewer}
import starling.services.rpc.refdata._
import starling.services.rabbit._
import collection.immutable.Map
import com.trafigura.services.trinity.TrinityService
import com.trafigura.services.marketdata.{ExampleService, MarketDataServiceApi}
import starling.fc2.api.FC2Service
import starling.dbx.DataSourceFactory
import starling.titan.{TitanTradeCache, TitanSystemOfRecord, TitanTradeStore}
import java.util.UUID
import com.trafigura.services.{DocumentationService, WebServiceFactory, ResteasyServiceApi}
import starling.instrument.utils.StarlingXStream
import java.util.concurrent.{Executors, ConcurrentHashMap}
import swing.event.Event
import starling.utils.ClosureUtil._
import starling.browser.service.{BrowserService, UserLoggedIn, Version}
import starling.databases.utils.{RabbitBroadcaster, RabbitMessageSender}
import starling.auth.{AuthHandler, LdapUserLookup, User}
import starling.auth.internal.KerberosAuthHandler
import starling.manager.BromptonContext
import starling.utils._
import starling.eai.{EAIDealBookMapping, Traders, EAIAutoImport, EAIStrategyDB}


class StarlingInit( val props: Props,
                    authHandler:AuthHandler = AuthHandler.Dev,
                    rmiBroadcaster:Broadcaster = Broadcaster.Null,
                    dbMigration: Boolean = true,
                    startRMI: Boolean = true, //not used
                    startHttp: Boolean = true,
                    startXLLoop: Boolean = true,
                    startStarlingJMX: Boolean = true,
                    forceGUICompatability: Boolean = true,
                    startEAIAutoImportThread: Boolean = true,
                    startRabbit: Boolean = true,
                    testMode : Boolean = false,
                    marketDataReadersProviders:MarketDataPageIdentifierReaderProviders = MarketDataPageIdentifierReaderProviders.Empty
                    ) extends Stopable with Log {

  private lazy val services = CompositeStopable(
    // Load all user settings, pivot layouts and user reports here to ensure we don't have any de-serialization issues. Do this before anyone can connect.
    true                     → List(Stopable(userSettingsDatabase.readAll _)),
    startRabbit              → List(titanRabbitEventServices),
    startXLLoop              → List(excelLoopReceiver),
    startHttp                → List(httpServer, webServiceServer, trinityService),
    startStarlingJMX         → List(jmx),
    startEAIAutoImportThread → List(eaiAutoImport, fwdCurveAutoImport),
    true                     → List(scheduler, Stopable(stopF = DataSourceFactory.shutdown _))
  )

  override def start = { services.start }
  override def stop =  { services.stop  }

  val runtime = Runtime.getRuntime
  log.info("maxMemory %sMB" % (runtime.maxMemory / 1024 / 1024))

  val name = props.ServerName()
  val ldapUserLookup = new LdapUserLookup

  log.debug("Initializing basic DB connections...")
  val starlingDB = DB(props.StarlingDatabase())
//  starlingDB.setReadCommittedSnapshot
  val trinityDB = DB(props.TrinityDatabase())
  val galenaDB = DB(props.GalenaDatabase())
  val varSqlServerDB = DB(props.VarSqlServer())
  val eaiSqlServerDB = DB(props.EAIReplica())
  val eaiStarlingSqlServerDB = DB(props.EAIStarlingSqlServer())

  log.debug("Initializing: Things that need access to a DB like portfolios, holidays, markets, indexes...")
  val holidayTables = new Holidays(eaiSqlServerDB)
  HolidayTablesFactory.registerHolidayTablesImpl(holidayTables)
  val businessCalendars = new BusinessCalendars(holidayTables)
  val expiryRules = new FuturesExpiryRulesImpl(eaiSqlServerDB, businessCalendars)
  FuturesExpiryRuleFactory.registerRulesImpl(expiryRules)
  MarketProvider.registerCreator(new MarketLookupCreator {
    def create = new StarlingMarketLookup(starlingDB, businessCalendars, expiryRules)
  })
  val richResultSetRowFactory = new RichResultSetRowFactory

  log.debug("Initializing: Rich DB Connections. Which use things like market factories for smarter deserialization...")
  val starlingRichDB = new RichDB(props.StarlingDatabase(), richResultSetRowFactory)
  val trintityRichDB = new RichDB(props.TrinityDatabase(), richResultSetRowFactory)
  val softmarRichDB = new RichDB(props.SoftmarDatabase(), richResultSetRowFactory)
  val neptuneRichDB = new RichDB(props.NeptuneDatabase(), richResultSetRowFactory)

  val galenaRichDB = new RichDB(props.GalenaDatabase(), richResultSetRowFactory)
  val varSQLServerRichDB = new RichDB(props.VarSqlServer(), richResultSetRowFactory)
  val eaiRichSqlServerDB = new RichDB(props.EAIReplica(), richResultSetRowFactory)
  val eaiStarlingRichSqlServerDB = new RichDB(props.EAIStarlingSqlServer(), richResultSetRowFactory)

  // The Market data store needs some of it's own xstream converters
  MarketDataXStreamConverters.converters.foreach(StarlingXStream.registerConverter)

  val mailSender = new JavaMailSenderImpl().update(_.setHost(props.SmtpServerHost()), _.setPort(props.SmtpServerPort()))

  val trinityService = new TrinityService(ResteasyServiceApi(props.TrinityServiceUrl()))

  val titanRabbitEventServices = if (!testMode) {
    new DefaultTitanRabbitEventServices(props)
  }
  else {
    new MockTitanRabbitEventServices()
  }

  log.debug("Completed rabbit start")

  val broadcaster = ObservingBroadcaster(new CompositeBroadcaster(
      props.rabbitHostSet                       → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
      props.EnableVerificationEmails()          → new EmailBroadcaster(mailSender),
      (startRabbit && props.titanRabbitHostSet) → TitanRabbitIdBroadcaster(titanRabbitEventServices.rabbitEventPublisher),
      true                                      → rmiBroadcaster
  ))

  val revalSnapshotDb = new RevalSnapshotDB(starlingDB)
  val limServer = new LIMServer(props.LIMHost(), props.LIMPort())

  lazy val (fwdCurveAutoImport, marketDataStore) = log.infoWithTime("Creating Market Data Store") {
    import MarketDataSet._

    val marketDataSources = Map(
      LimMetals → new RefinedMetalsLimMarketDataSource(limServer),
      LIM → new OilAndMetalsVARLimMarketDataSource(limServer),
      System → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 1),
      Crude → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 5),
      LondonDerivatives → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 4),
      BarryEckstein → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 3),
      GasolineRoW → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 21),
      LondonDerivativesOptions → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 45),
      //TrinityLive → new TrinityMarketDataSource(trintityRichDB, BradyProfilePricingGroup.liveBradyProfilePricingGroup),
      //GalenaLive → new GalenaMarketDataSource(galenaRichDB, BradyProfilePricingGroup.liveBradyProfilePricingGroup),
      //GalenaFullCurve → new GalenaMarketDataSource(galenaRichDB, BradyProfilePricingGroup.fullCurveBradyProfilePricingGroup),
      TrinityDiscountFactorCSV → new TrinityDiscountFactorCSVDataSource()//,
      //Neptune -> new NeptuneBenchmarksMarketDataSource(neptuneRichDB) I don't want this persisted yet as it is likely to change
    )

    lazy val mds = DBMarketDataStore(props, starlingRichDB, marketDataSources, broadcaster)

    val fwdCurveAutoImport = new FwdCurveAutoImport(60*15, mds, marketDataSources.flatMap {
      case (k, f: FwdCurveDbMarketDataSource) => Some(k)
      case _ => None
    }.toSet, businessCalendars.US_UK)
    (fwdCurveAutoImport, mds)
  }

  if (dbMigration) log.infoWithTime("DB Migration") {
    //Ensure the schema is up to date
    new PatchRunner(starlingRichDB, props.ReadonlyMode(), this).updateSchemaIfRequired
  }

  val userSettingsDatabase = new UserSettingsDatabase(starlingDB, broadcaster)
  val rabbitEventDatabase = new RabbitEventDatabase(starlingDB, broadcaster)

  val strategyDB = new EAIStrategyDB(eaiSqlServerDB)
  val eaiDealBookMapping = new EAIDealBookMapping(eaiSqlServerDB)

  val intradayTradesDB = new IntradayTradeStore(starlingRichDB, strategyDB, broadcaster, ldapUserLookup)

  // Brady trade stores, system of records, trade importers

  val eaiTradeStores = Desk.eaiDesks.map{case desk@Desk(_, _, Some(_:EAIDeskInfo)) => desk -> new EAITradeStore(starlingRichDB, broadcaster, strategyDB, desk)}.toMap

  val titanTradeStore = new TitanTradeStore(starlingRichDB, broadcaster, TitanTradeSystem)

  val refinedAssignmentTradeStore = new RefinedAssignmentTradeStore(starlingRichDB, broadcaster)
  val refinedAssignmentSystemOfRecord = new RefinedAssignmentSystemOfRecord(neptuneRichDB)
  val refinedAssignmentImporter = new TradeImporter(refinedAssignmentSystemOfRecord, refinedAssignmentTradeStore)

  val refinedFixationTradeStore = new RefinedFixationTradeStore(starlingRichDB, broadcaster)
  val refinedFixationSystemOfRecord = new RefinedFixationSystemOfRecord(neptuneRichDB)
  val refinedFixationImporter = new TradeImporter(refinedFixationSystemOfRecord, refinedFixationTradeStore)

  val tradeImporterFactory = new TradeImporterFactory(refinedAssignmentImporter, refinedFixationImporter)

  val enabledDesks: Set[Desk] = props.EnabledDesks().trim.toLowerCase match {
    case "" => throw new Exception("EnabledDesks property not set, valid values: all, none, " + Desk.names.mkString(", "))
    case "all" => Desk.values.toSet
    case "none" => Set.empty
    case names => names.split(",").toList.map(Desk.fromName).toSet
  }

  val closedDesks = new ClosedDesks(broadcaster, starlingDB)

  val (titanTradeCache : TitanTradeCache, titanServices, logisticsServices, titanInventoryCache) = if (!testMode) {
    (
      new DefaultTitanTradeCache(props),
      new DefaultTitanServices(props),
      new DefaultTitanLogisticsServices(props),
      new DefaultTitanLogisticsInventoryCache(props)
    )
  }
  else {
    val fileMockedTitanServices = new FileMockedTitanServices()
    val fileMockedTitanLogisticsServices = new FileMockedTitanLogisticsServices()
    val mockTitanTradeService = new DefaultTitanTradeService(fileMockedTitanServices)
    (
      new TitanTradeServiceBasedTradeCache(mockTitanTradeService),
      fileMockedTitanServices,
      fileMockedTitanLogisticsServices,
      new TitanLogisticsServiceBasedInventoryCache(fileMockedTitanLogisticsServices)
    )
  }

  val valuationService = new ValuationService(new DefaultEnvironmentProvider(marketDataStore), titanTradeCache, titanServices, logisticsServices, titanRabbitEventServices, titanInventoryCache, Some(titanTradeStore), rabbitEventDatabase)
  val marketDataService = new MarketDataService(marketDataStore, new DefaultEnvironmentProvider(marketDataStore))

  val titanSystemOfRecord = new TitanSystemOfRecord(titanTradeCache, titanServices, logisticsServices)
  val titanTradeImporter = new TradeImporter(titanSystemOfRecord, titanTradeStore)

  val tradeImporters = Map[TradeSystem,TradeImporter](
    RefinedAssignmentTradeSystem → refinedAssignmentImporter,
    RefinedFixationTradeSystem → refinedFixationImporter,
    TitanTradeSystem → titanTradeImporter)

  val tradeStores = new TradeStores(
    tradeImporters,
    closedDesks,
    eaiTradeStores, intradayTradesDB,
    refinedAssignmentTradeStore, refinedFixationTradeStore, titanTradeStore)


  val hostname = try {
    InetAddress.getLocalHost.getHostName
  } catch {
    case _ => "Unknown Host"
  }

  private val production = props.Production()
  private val serverName = props.ServerName()
  private val guiColour = if (production || props.UseProductionColour()) None else Some(PropsHelper.createColour(serverName))
  val version = Version(serverName, hostname, props.StarlingDatabase().url, production, guiColour)

  val curveViewer = new CurveViewer(marketDataStore)
  val trinityUploader = new TrinityUploader(new FCLGenerator(businessCalendars, curveViewer),
    new XRTGenerator(marketDataStore), trinityService, props)

  val scheduler = Scheduler.create(businessCalendars, marketDataStore, broadcaster, trinityUploader, props)

  val referenceData = new ReferenceData(businessCalendars, marketDataStore, strategyDB, scheduler)


  val traders = new Traders(ldapUserLookup.user _)

  val starlingServer = new StarlingServerImpl(name,
    userSettingsDatabase, tradeStores, enabledDesks,
    version, referenceData, businessCalendars.UK, ldapUserLookup, eaiStarlingSqlServerDB, traders, rabbitEventDatabase)

  val fc2Service = new FC2ServiceImpl(marketDataStore, curveViewer, marketDataReadersProviders)

  val rmiPort = props.RmiPort()

  val jmx = new StarlingJMX(new ConcurrentHashMap[UUID,User], scheduler) //FIXME

  def currentUser() = User.currentlyLoggedOn

  val excelLoopReceiver = new ExcelLoopReceiver(ldapUserLookup, props.XLLoopPort(),
    new MarketDataHandler(broadcaster, starlingServer, marketDataStore),
    new TradeHandler(broadcaster, new ExcelTradeReader(strategyDB, eaiDealBookMapping, traders, currentUser), intradayTradesDB, traders),
    new DiagnosticHandler(starlingServer))

  val loopyXLReceivers = new CurveHandler(curveViewer, marketDataStore) :: excelLoopReceiver.handlers.toList

  val browserService = new BrowserServiceImpl(name, version, userSettingsDatabase, broadcaster)

  val eaiAutoImport = new EAIAutoImport(15, starlingRichDB, eaiStarlingRichSqlServerDB, strategyDB, eaiTradeStores, closedDesks, enabledDesks)

  val deltaCSVDir = props.DeltaCSVDir()

  lazy val httpServer = locally {
    val externalURL = props.ExternalUrl()
    val externalHostname = props.ExternalHostname()
    val xlloopUrl = props.XLLoopUrl()
    val rmiPort = props.RmiPort()
    val webStartServlet = new WebStartServlet("webstart", serverName, externalURL, "starling.launcher.Launcher",
      List(externalHostname, rmiPort.toString), xlloopUrl)
    val cannedWebStartServlet = new WebStartServlet("cannedwebstart", serverName, externalURL,
      "starling.gui.CannedLauncher", List(), xlloopUrl)

    new HttpServer(props.HttpPort(), props.ExternalUrl(), props.ServerName(), None, Nil,
      "webstart"       → webStartServlet,
      "cannedwebstart" → cannedWebStartServlet)
  }

  lazy val webServiceServer = locally {
    val webXmlUrl = "services/resources/webapp/WEB-INF/web.xml"//classOf[StarlingInit].getResource("../../webapp/WEB-INF/web.xml").toExternalForm

    new HttpServer(props.HttpServicePort(), props.HttpServiceExternalUrl(), serverName, Some(webXmlUrl), Nil) {
      override def start =
        log.infoF("HTTP web service external url = '%s', server name = '%s'" % (props.HttpServiceExternalUrl(), serverName)) {
          super.start; /*DocumentationService.registerInstances(webServices : _*) */
        }
    }
  }

  StarlingWebServices.webServices = List(marketDataService, valuationService, /*DocumentationService, */ExampleService)
}

class StarlingWebServices extends WebServiceFactory {
  override val services = {
    if (StarlingWebServices.webServices == null) {
      throw new Exception("StarlingWebServices.services has not been initialised")
    } else {
      StarlingWebServices.webServices
    }
  }
}

object StarlingWebServices {
  var webServices:List[AnyRef] = _
}

object StarlingInit {

  lazy val devInstance = new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, false, false, false, false, false, false, false)

  lazy val runningDevInstance = {
    devInstance.update(_.start)
  }

  /**
   * defines a test service instance that runs RMI but uses mocked service stubs where appropriate
   */
  lazy val runningTestInstance = {
    new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, true, true, false, false, false, false, false, false, true).update(_.start)
  }
}