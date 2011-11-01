package starling.services

import excel._
import starling.schemaevolution.system.PatchRunner
import starling.db._
import starling.richdb.{RichDB, RichResultSetRowFactory}
import starling.market._
import starling.props.{PropsHelper, Props}
import java.net.InetAddress
import starling.curves.readers._
import lim.{PriceFixingLimMarketDataSource, SpotFXLimMarketDataSource, PriceLimMarketDataSource}
import starling.utils.ImplicitConversions._
import starling.http._
import starling.rmi._
import starling.calendar._
import collection.immutable.Map
import starling.dbx.DataSourceFactory
import starling.instrument.utils.StarlingXStream
import starling.browser.service.Version
import starling.auth.{AuthHandler, User}
import starling.utils._
import starling.auth.internal.LdapUserLookupImpl
import starling.curves._
import org.apache.commons.io.IOUtils
import starling.marketdata.{MarketDataTypes, DBReferenceDataLookup}
import starling.lim.LIMService
import starling.gui.api.Email


class StarlingInit( val props: Props,
                    authHandler:AuthHandler = AuthHandler.Dev,
                    rmiBroadcaster:Broadcaster = Broadcaster.Null,
                    dbMigration: Boolean = true,
                    startRMI: Boolean = true, //not used
                    startHttp: Boolean = true, //not used
                    startStarlingJMX: Boolean = true,
                    forceGUICompatability: Boolean = true,
                    startEAIAutoImportThread: Boolean = true,
                    testMode : Boolean = false,
                    marketDataReadersProviders:MarketDataPageIdentifierReaderProviders = MarketDataPageIdentifierReaderProviders.Empty
                    ) extends Stopable with Log {

  private lazy val services = CompositeStopable(
    true                     → List(Stopable(userSettingsDatabase.readAll _)),
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

  if (dbMigration) log.infoWithTime("DB Migration") {
    //Ensure the schema is up to date
    val requiresRestart = new PatchRunner(starlingRichDB, props.ReadonlyMode(), this).updateSchemaIfRequired
    if(requiresRestart) {
      Log.warn("A patch applied required a restart. Exiting Starling")
      System.exit(1)
    }
  }

  val revalSnapshotDb = new RevalSnapshotDB(starlingDB)

  lazy val referenceDataLookup = DBReferenceDataLookup(neptuneRichDB)
  lazy val dataTypes = new MarketDataTypes(referenceDataLookup)

  lazy val emailService = new PersistingEmailService(
    rmiBroadcaster, DB(props.StarlingDatabase()), props.SmtpServerHost(), props.SmtpServerPort()) with FooterAppendingEmailService {

    val footer = Map("Sent by" → props.ServerType(), "Name" → props.ServerName(), "Host" → props.ExternalHostname())
  }

  lazy val bloombergImports = DBBloombergImports.importsFrom(DB(props.EAIReplica()))

  lazy val (fwdCurveAutoImport, marketDataStore) = log.infoWithTime("Creating Market Data Store") {
    import MarketDataSet._
    val limService = LIMService(props.LIMHost(), props.LIMPort())
    val template = Email(props.MetalsEmailAddress(), props.LimEmailAddress())
    val verificationEmailService = emailService.enabledIf(props.EnableVerificationEmails())

    val marketDataSources = MultiMap[MarketDataSet, MarketDataSource](
      LimMetals ->> (new PriceLimMarketDataSource(limService, bloombergImports, verificationEmailService, template),
                     new SpotFXLimMarketDataSource(limService, verificationEmailService, template),
                     new PriceFixingLimMarketDataSource(limService,verificationEmailService, template)),
      LIM ->> new OilAndMetalsVARLimMarketDataSource(limService),
      System ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 1),
      Crude ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 5),
      LondonDerivatives ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 4),
      BarryEckstein ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 3),
      GasolineRoW ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 21),
      GasOil ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 7),
      Naphtha ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 10),
      LondonDerivativesOptions ->> new FwdCurveDbMarketDataSource(varSqlServerDB, businessCalendars, 45)
//      Neptune ->> (new NeptuneGradeAreaBenchmarksMarketDataSource(neptuneRichDB),
//                   new NeptuneCountryBenchmarksMarketDataSource(neptuneRichDB),
//                   new NeptuneFreightParityMarketDataSource(neptuneRichDB))
    ).withDefaultValue(Nil)

    lazy val mds = Log.infoWithTime("Creating DBMarketDataStore") {
      new DBMarketDataStore(new NewSchemaMdDB(starlingRichDB, dataTypes), new MarketDataSnapshots(starlingRichDB),
        marketDataSources, rmiBroadcaster, dataTypes)
    }

    val fwdCurveAutoImport = new FwdCurveAutoImport(60*15, mds,
      marketDataSources.filterValues(_.isInstanceOf[FwdCurveDbMarketDataSource]).keySet, businessCalendars.US_UK)

    (fwdCurveAutoImport, mds)
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
  val gitCommit = {
    try {
      val process = Runtime.getRuntime.exec("git log -1")
      val stdOut = IOUtils.toByteArray(process.getInputStream)
      IOUtils.toByteArray(process.getErrorStream)
      process.waitFor
      if (process.exitValue == 0) {
        val lines = new String(stdOut).split("\n")
        lines.headOption match {
          case None => "Can't find commit id from log"
          case Some(t) => t.replaceFirst("commit ", "")
        }
      } else {
        "git returned non zero exit code"
      }
    } catch {
      case e => "git not found on path"
    }
  }
  val version = Version(serverName, hostname, props.StarlingDatabase().url, gitCommit, production, guiColour)

  val environmentRules = new EnvironmentRules
  val curveViewer = new CurveViewer(marketDataStore, environmentRules)

  val referenceDataService = new ReferenceDataService()
  referenceDataService.add(ReferenceData("Calendars", ReferenceDataService.calendars(businessCalendars)))
  referenceDataService.add(ReferenceData("Pricing Groups", ReferenceDataService.pricingGroups(marketDataStore)))

  val starlingServer = new StarlingServerImpl(name,
    userSettingsDatabase,
    version, referenceDataService, businessCalendars.UK)

  val fC2Service = new FC2Service(marketDataStore, dataTypes, marketDataReadersProviders)
  val fCFacility = new FC2FacilityImpl(fC2Service, marketDataStore, curveViewer, environmentRules)

  def currentUser() = User.currentlyLoggedOn

  val marketDataHandler = new MarketDataHandler(rmiBroadcaster, starlingServer, marketDataStore)
  val diagnosticHandler = new DiagnosticHandler(starlingServer)

  val xlloopAndloopyReceivers = marketDataHandler :: diagnosticHandler :: Nil
  val curveHandler = new CurveHandler(curveViewer, marketDataStore)

  val browserService = new BrowserServiceImpl(name, version, userSettingsDatabase, rmiBroadcaster)

  val servlets = {
    val externalURL = props.ExternalUrl()
    val externalHostname = props.ExternalHostname()
    val xlloopUrl = props.XLLoopUrl()
    val rmiPort = props.RmiPort()

    val webStartServlet = new WebStartServlet("webstart", serverName, externalURL, "starling.launcher.Launcher",
      List(externalHostname, rmiPort.toString), xlloopUrl)
    val cannedWebStartServlet = new WebStartServlet("cannedwebstart", serverName, externalURL,
      "starling.gui.CannedLauncher", List(), xlloopUrl)

    Map("webstart"       → webStartServlet, "cannedwebstart" → cannedWebStartServlet)
  }

}

object StarlingInit {

  lazy val devInstance = new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, false, false, false, false, false, false)

  lazy val runningDevInstance = {
    devInstance.update(_.start)
  }

  /**
   * defines a test service instance that runs RMI but uses mocked service stubs where appropriate
   */
  lazy val runningTestInstance = {
    new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, true, true, false, false, false, false, false).update(_.start)
  }
}
