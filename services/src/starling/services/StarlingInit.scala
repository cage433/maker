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
import starling.auth.{LdapUserLookup, User, ServerLogin}
import starling.dbx.ConnectionParams
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
import com.trafigura.services.valuation.{ValuationServiceApi, TradeManagementCacheNotReady}
import org.jboss.netty.channel.{ChannelLocal, Channel}
import starling.curves.{StarlingMarketLookup, FwdCurveAutoImport, CurveViewer}
import starling.services.rpc.refdata._
import starling.services.rabbit._
import collection.immutable.Map
import com.trafigura.services.trinity.TrinityService
import com.trafigura.services.marketdata.{ExampleService, MarketDataServiceApi}
import starling.fc2.api.FC2Service
import starling.utils._
import starling.browser.service.{BrowserService, UserLoggedIn, Version}
import starling.dbx.DataSourceFactory
import starling.titan.{TitanTradeCache, TitanSystemOfRecord, TitanTradeStore}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import com.trafigura.services.{DocumentationService, WebServiceFactory, ResteasyServiceApi}
import starling.instrument.utils.StarlingXStream

class StarlingInit( val props: Props,
                    dbMigration: Boolean = true,
                    startRMI: Boolean = true,
                    startHttp: Boolean = true,
                    startXLLoop: Boolean = true,
                    startStarlingJMX: Boolean = true,
                    forceGUICompatability: Boolean = true,
                    startEAIAutoImportThread: Boolean = true,
                    startRabbit: Boolean = true,
                    testMode : Boolean = false
                    ) extends Stopable with Log {

  implicit def enrichBouncyServer(bouncy: BouncyRMIServer[_]) = new {
    def toStoppable = Stopable(
      () => {log.info(bouncy.name + " RMI started on port " + bouncy.port); bouncy.start}, () => bouncy.stop())
  }

  private lazy val services = CompositeStopable(
    // Load all user settings, pivot layouts and user reports here to ensure we don't have any de-serialization issues. Do this before anyone can connect.
    true                     → List(Stopable(userSettingsDatabase.readAll _)),
    startRabbit              → List(titanRabbitEventServices),
    startXLLoop              → List(excelLoopReceiver, loopyXLReceiver),
    startRMI                 → List(rmiServerForTitan.toStoppable, rmiServerForGUI.toStoppable),
    startHttp                → List(httpServer, regressionServer, webServiceServer, trinityService),
    startStarlingJMX         → List(jmx),
    startEAIAutoImportThread → List(eaiAutoImport, fwdCurveAutoImport),
    true                     → List(scheduler, Stopable(stopF = DataSourceFactory.shutdown _))
  )

  override def start = { super.start; services.start }
  override def stop =  { super.stop;  services.stop  }

  val runtime = Runtime.getRuntime
  log.info("maxMemory %sMB" % (runtime.maxMemory / 1024 / 1024))

  val name = props.ServerName()
  val ldapUserLookup = new LdapUserLookup with BouncyLdapUserLookup[User]

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
      true                                      → new RMIBroadcaster(rmiServerForGUI),
      props.rabbitHostSet                       → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
      props.EnableVerificationEmails()          → new EmailBroadcaster(mailSender),
      (startRabbit && props.titanRabbitHostSet) → TitanRabbitIdBroadcaster(titanRabbitEventServices.rabbitEventPublisher)))

  val revalSnapshotDb = new RevalSnapshotDB(starlingDB)
  val limServer = new LIMServer(props.LIMHost(), props.LIMPort())

  val (fwdCurveAutoImport, marketDataStore) = log.infoWithTime("Creating Market Data Store") {
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

    val mds = new DBMarketDataStore(starlingRichDB, marketDataSources, broadcaster)

    val fwdCurveAutoImport = new FwdCurveAutoImport(60*15, mds, marketDataSources.flatMap {
      case (k, f: FwdCurveDbMarketDataSource) => Some(k)
      case _ => None
    }.toSet, businessCalendars.US_UK)
    (fwdCurveAutoImport, mds)
  }

  val userSettingsDatabase = new UserSettingsDatabase(starlingDB, broadcaster)

  if (dbMigration) log.infoWithTime("DB Migration") {
    //Ensure the schema is up to date
    new PatchRunner(starlingRichDB, props.ReadonlyMode(), this).updateSchemaIfRequired
  }

  val strategyDB = new EAIStrategyDB(eaiSqlServerDB)

  val intradayTradesDB = new IntradayTradeStore(starlingRichDB, strategyDB, broadcaster, ldapUserLookup)

  // Brady trade stores, system of records, trade importers

  val eaiTradeStores = Book.all.map(book=> book-> new EAITradeStore(starlingRichDB, broadcaster, strategyDB, book)).toMap

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

  val valuationService = new ValuationService(new DefaultEnvironmentProvider(marketDataStore), titanTradeCache, titanServices, logisticsServices, titanRabbitEventServices, titanInventoryCache)
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

  val reportContextBuilder = new ReportContextBuilder(marketDataStore)
  val reportService = new ReportService(reportContextBuilder,tradeStores)

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

  val userReportsService = new UserReportsService(businessCalendars.UK, tradeStores, marketDataStore, userSettingsDatabase, reportService)

  val traders = new Traders(ldapUserLookup.user _)

  val starlingServer = new StarlingServerImpl(name, reportContextBuilder, reportService,
    userSettingsDatabase, userReportsService, tradeStores, enabledDesks,
    version, referenceData, businessCalendars.UK, ldapUserLookup, eaiStarlingSqlServerDB, traders)

  val fc2Service = new FC2ServiceImpl(marketDataStore, curveViewer, reportService)

  val rmiPort = props.RmiPort()

  val users = new ConcurrentHashMap[UUID,User]

  val jmx = new StarlingJMX(users, scheduler)

  val auth:ServerAuthHandler[User] = props.UseAuth() match {
    case false => {
      log.warn("Auth disabled")
      new ServerAuthHandler[User](new NullAuthHandler(Some(User.Dev)), users, ldapUserLookup,
        user => broadcaster.broadcast(UserLoggedIn(user.name)), ChannelLoggedIn)
    }
    case true => {
      val kerberos = new ServerLogin(props.KerberosPassword())
      new ServerAuth(kerberos, ldapUserLookup, users, user => broadcaster.broadcast(UserLoggedIn(user.name))).handler
    }
  }

  def currentUser() = User.currentlyLoggedOn

  val excelLoopReceiver = new ExcelLoopReceiver(ldapUserLookup, props.XLLoopPort(),
    new MarketDataHandler(broadcaster, starlingServer, marketDataStore),
    new TradeHandler(broadcaster, new ExcelTradeReader(strategyDB, traders, currentUser), intradayTradesDB, traders),
    new ReportHandler(userReportsService),
    new DiagnosticHandler(starlingServer))

  val loopyXLReceiver = new LoopyXLReceiver(props.LoopyXLPort(), auth,
    (new CurveHandler(curveViewer, marketDataStore) :: excelLoopReceiver.handlers.toList) : _*)

   val latestTimestamp = if (forceGUICompatability) GUICode.latestTimestamp.toString else BouncyRMI.CodeVersionUndefined

  val browserService = new BrowserServiceImpl(name, version, userSettingsDatabase)

  val rmiServerForGUI:BouncyRMIServer[User] = new BouncyRMIServer(
    rmiPort,
    auth, latestTimestamp, users,
    Set(classOf[UnrecognisedTradeIDException]),
    ChannelLoggedIn, "GUI",
    ThreadNamingProxy.proxy(starlingServer, classOf[StarlingServer]),
    ThreadNamingProxy.proxy(fc2Service, classOf[FC2Service]),
    ThreadNamingProxy.proxy(browserService, classOf[BrowserService])
  )

  log.info("Initialize public services for Titan components, service host/port: " + props.ExternalHostname + "/" + props.StarlingServiceRmiPort)
  val nullHandler = new ServerAuthHandler[User](new NullAuthHandler(Some(User.Dev)), users, ldapUserLookup,
        user => broadcaster.broadcast(UserLoggedIn(user.name)), ChannelLoggedIn)
  val rmiServerForTitan : BouncyRMIServer[User] = new BouncyRMIServer(
    props.StarlingServiceRmiPort(),
    nullHandler, BouncyRMI.CodeVersionUndefined, users,
    Set(classOf[TradeManagementCacheNotReady], classOf[IllegalArgumentException]),
    ChannelLoggedIn, "Titan",
    ThreadNamingProxy.proxy(valuationService, classOf[ValuationServiceApi]),
    ThreadNamingProxy.proxy(marketDataService, classOf[MarketDataServiceApi])
  )

  val eaiAutoImport = new EAIAutoImport(15, starlingRichDB, eaiStarlingRichSqlServerDB, strategyDB, eaiTradeStores, closedDesks, enabledDesks)

  val deltaCSVDir = props.DeltaCSVDir()

  val reportServlet = new ReportServlet("reports", userReportsService) //Don't add to main web server (as this has no authentication)

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

  lazy val webServices = List(marketDataService, valuationService, /*DocumentationService, */ExampleService)
  lazy val regressionServer = new RegressionServer(props.RegressionPort(), reportServlet)
}

class StarlingWebServices extends WebServiceFactory {
  override val services = Server.server.webServices
}

object StarlingInit{
  lazy val devInstance = {
    new StarlingInit(PropsHelper.defaultProps, true, false, false, false, false, false, false, false).update(_.start)
  }

  /**
   * defines a test service instance that runs RMI but uses mocked service stubs where appropriate
   */
  lazy val testInstance = {
    new StarlingInit(PropsHelper.defaultProps, true, true, false, false, false, false, false, false, true).update(_.start)
  }
}

object ChannelLoggedIn extends LoggedIn[User,UUID] {
  private val loggedIn = new ChannelLocal[(User,UUID)]()

  def get(channel: Channel) = loggedIn.get(channel)
  def set(channel: Channel, user: User, id:UUID) = loggedIn.set(channel, (user,id))
  def remove(channel: Channel) = loggedIn.remove(channel)
  def setLoggedOn(user: Option[User]) {
    User.setLoggedOn(user)
  }
}
