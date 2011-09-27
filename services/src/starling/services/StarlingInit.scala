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
import java.util.UUID
import com.trafigura.services.{DocumentationService, WebServiceFactory, ResteasyServiceApi}
import starling.instrument.utils.StarlingXStream
import java.util.concurrent.{Executors, ConcurrentHashMap}
import swing.event.Event
import starling.utils.ClosureUtil._
import starling.browser.service.{BrowserService, UserLoggedIn, Version}
import starling.databases.utils.{RabbitBroadcaster, RabbitMessageSender}
import starling.auth.{AuthHandler, LdapUserLookup, User}
import starling.manager.BromptonContext
import starling.utils._
import starling.eai.{EAIDealBookMapping, Traders, EAIAutoImport, EAIStrategyDB}
import starling.titan._
import starling.auth.internal.{LdapUserLookupImpl, KerberosAuthHandler}

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
    true                     → List(Stopable(userSettingsDatabase.readAll _)),
    startXLLoop              → List(excelLoopReceiver),
    startHttp                → List(httpServer),
    startEAIAutoImportThread → List(/*eaiAutoImport, */fwdCurveAutoImport), //FIXME
    true                     → List(Stopable(stopF = DataSourceFactory.shutdown _))
  )

  override def start = { services.start }
  override def stop =  { services.stop  }

  val runtime = Runtime.getRuntime
  log.info("maxMemory %sMB" % (runtime.maxMemory / 1024 / 1024))

  val name = props.ServerName()
  val ldapUserLookup = new LdapUserLookupImpl

  log.debug("Initializing basic DB connections...")
  val starlingDB = DB(props.StarlingDatabase())
//  starlingDB.setReadCommittedSnapshot
  val trinityDB = DB(props.TrinityDatabase())
  val galenaDB = DB(props.GalenaDatabase())
  val varSqlServerDB = DB(props.VarSqlServer())
  val eaiSqlServerDB = DB(props.EAIReplica())

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

  val eaiStarlingRichSqlServerDB = new RichDB(props.EAIStarlingSqlServer(), richResultSetRowFactory)

  // The Market data store needs some of it's own xstream converters
  MarketDataXStreamConverters.converters.foreach(StarlingXStream.registerConverter)

  log.debug("Completed rabbit start")

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
      GasOil → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 7),
      LondonDerivativesOptions → new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 45),
      //TrinityLive → new TrinityMarketDataSource(trintityRichDB, BradyProfilePricingGroup.liveBradyProfilePricingGroup),
      //GalenaLive → new GalenaMarketDataSource(galenaRichDB, BradyProfilePricingGroup.liveBradyProfilePricingGroup),
      //GalenaFullCurve → new GalenaMarketDataSource(galenaRichDB, BradyProfilePricingGroup.fullCurveBradyProfilePricingGroup),
      TrinityDiscountFactorCSV → new TrinityDiscountFactorCSVDataSource()//,
      //Neptune -> new NeptuneBenchmarksMarketDataSource(neptuneRichDB) I don't want this persisted yet as it is likely to change
    )

    lazy val mds = DBMarketDataStore(props, starlingRichDB, marketDataSources, rmiBroadcaster)

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

  val userSettingsDatabase = new UserSettingsDatabase(starlingDB, rmiBroadcaster)

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

  val referenceData = new ReferenceData(businessCalendars, marketDataStore/*, scheduler*/) //add scheduler

  val starlingServer = new StarlingServerImpl(name,
    userSettingsDatabase,
    version, referenceData, businessCalendars.UK)

  val fc2Service = new FC2ServiceImpl(marketDataStore, curveViewer, marketDataReadersProviders)

  def currentUser() = User.currentlyLoggedOn

  val excelLoopReceiver = new ExcelLoopReceiver(ldapUserLookup, props.XLLoopPort(),
    new MarketDataHandler(rmiBroadcaster, starlingServer, marketDataStore),
    new DiagnosticHandler(starlingServer))

  val loopyXLReceivers = new CurveHandler(curveViewer, marketDataStore) :: excelLoopReceiver.handlers.toList

  val browserService = new BrowserServiceImpl(name, version, userSettingsDatabase, rmiBroadcaster)

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