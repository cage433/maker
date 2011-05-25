package starling.services

import excel._
import jmx.StarlingJMX
import starling.schemaevolution.system.PatchRunner
import starling.db._
import starling.richdb.{RichDB, RichResultSetRowFactory}
import starling.market._
import rules.MarketPrecisionFactory
import starling.props.{PropsHelper, Props}
import starling.tradestore.eai.EAITradeStore
import java.net.InetAddress
import starling.tradestore.intraday.IntradayTradeStore
import starling.neptune.{RefinedFixationSystemOfRecord, RefinedFixationTradeStore, RefinedAssignmentSystemOfRecord, RefinedAssignmentTradeStore}
import starling.utils.ImplicitConversions._
import starling.curves.readers._
import trade.ExcelTradeReader
import trinity.{TrinityUploader, XRTGenerator, TrinityUploadCodeMapper, FCLGenerator}
import xml.{Node, Utility}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import java.io.{ByteArrayInputStream, File}
import javax.xml.transform.TransformerFactory
import starling.auth.{LdapUserLookup, User, ServerLogin}
import java.util.concurrent.CopyOnWriteArraySet
import starling.utils._
import sql.ConnectionParams
import starling.utils.ImplicitConversions._
import starling.tradeimport.{ClosedDesks, TradeImporterFactory, TradeImporter}
import starling.tradestore.TradeStores
import starling.http._
import starling.trade.TradeSystem
import starling.reports.pivot.{ReportContextBuilder, ReportService}
import starling.LIMServer
import org.mortbay.jetty.bio.{SocketConnector}
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import org.mortbay.jetty.{Server => JettyServer}
import starling.gui.api._
import starling.daterange.Day
import starling.calendar.{BusinessCalendar, DBHolidayTables, BusinessCalendars, HolidayTablesFactory}
import starling.bouncyrmi._
import starling.curves.{FwdCurveAutoImport, EAIQuotesFormulaIndexReader, CurveViewer}
import starling.eai.{Book, Traders, EAIAutoImport, EAIStrategyDB}
import com.jolbox.bonecp.BoneCP
import org.springframework.mail.javamail.{MimeMessageHelper, JavaMailSender, JavaMailSenderImpl}
import starling.rmi._


class StarlingInit( props: Props,
                    dbMigration: Boolean = true,
                    startRMI: Boolean = true,
                    startHttp: Boolean = true,
                    startXLLoop: Boolean = true,
                    startStarlingJMX: Boolean = true,
                    forceGUICompatability: Boolean = true,
                    startEAIAutoImportThread: Boolean = true
                    ) {

  def stop = {
    excelLoopReceiver.stop
    loopyXLReceiver.stop
    rmiServer.stop()
    httpServer.stop
    regressionServer.stop
    scheduler.stop
    regressionServer.stop
    ConnectionParams.shutdown
  }

  def start = {
    if (dbMigration) {
      //Ensure the schema is up to date
      new PatchRunner(starlingRichDB, props.ReadonlyMode(), this).updateSchemaIfRequired
    }

    locally {
      // Load all user settings, pivot layouts and user reports here to ensure we don't have any de-serialization issues. Do this before anyone can connect.
      userSettingsDatabase.readAll()
    }

    if (startXLLoop) {
      excelLoopReceiver.start
      loopyXLReceiver.start
    }

    if (startHttp) {
      httpServer.run
      regressionServer.start
    }

    if (startStarlingJMX) {
      jmx.start
    }

    if (startRMI) {
      rmiServer.start
      Log.info("RMI started on port " + rmiPort)
    }

    if (startEAIAutoImportThread) {
      eaiAutoImport.schedule
      fwdCurveAutoImport.schedule
    }

    scheduler.start
    regressionServer.start

    this
  }

  val runtime = Runtime.getRuntime
  Log.info("StarlingInit: maxMemory %sMB" % (runtime.maxMemory / 1024 / 1024))

  val name = props.ServerName()
  val ldapUserLookup = new LdapUserLookup

  // basic DB connections
  val starlingDB = DB(props.StarlingDatabase())
//  starlingDB.setReadCommittedSnapshot
  val trinityDB = DB(props.TrinityDatabase())
  val galenaDB = DB(props.GalenaDatabase())
  val varSqlServerDB = DB(props.VarSqlServer())
  val eaiSqlServerDB = DB(props.EAIReplica())
  val eaiStarlingSqlServerDB = DB(props.EAIStarlingSqlServer())

  // Things that need access to a DB like portfolios, holidays, markets, indexes...
  val holidayTables = new DBHolidayTables(eaiSqlServerDB)
  HolidayTablesFactory.registerHolidayTablesImpl(holidayTables)
  val businessCalendars = new BusinessCalendars(holidayTables)
  val expiryRules = new FuturesExpiryRulesImpl(eaiSqlServerDB, businessCalendars)
  FuturesExpiryRuleFactory.registerRulesImpl(expiryRules)
  val precisionRulesLoader = new PrecisionRulesLoader(eaiSqlServerDB)
  MarketPrecisionFactory.registerRulesImpl(precisionRulesLoader)
  val richResultSetRowFactory = new RichResultSetRowFactory
  val formulaIndexesReader = new EAIQuotesFormulaIndexReader(eaiSqlServerDB, businessCalendars)
  FormulaIndexList.set(Some(formulaIndexesReader))

  // Rich DB Connections. Which use things like market factories for smarter deserialization
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

  val broadcaster = new CompositeBroadcaster(
    true                             → new RMIBroadcaster(rmiServer),
    props.rabbitHostSet              → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
    props.EnableVerificationEmails() → new EmailBroadcaster(mailSender)
  )

  val strategyDB = new EAIStrategyDB(eaiSqlServerDB)

  val intradayTradesDB = new IntradayTradeStore(starlingRichDB, strategyDB, broadcaster, ldapUserLookup)

  // Brady trade stores, system of records, trade importers

  val eaiTradeStores = Book.all.map(book=> book-> new EAITradeStore(starlingRichDB, broadcaster, strategyDB, book)).toMap

  val refinedAssignmentTradeStore = new RefinedAssignmentTradeStore(starlingRichDB, broadcaster)
  val refinedAssignmentSystemOfRecord = new RefinedAssignmentSystemOfRecord(neptuneRichDB)
  val refinedAssignmentImporter = new TradeImporter(refinedAssignmentSystemOfRecord, refinedAssignmentTradeStore)

  val refinedFixationTradeStore = new RefinedFixationTradeStore(starlingRichDB, broadcaster)
  val refinedFixationSystemOfRecord = new RefinedFixationSystemOfRecord(neptuneRichDB)
  val refinedFixationImporter = new TradeImporter(refinedFixationSystemOfRecord, refinedFixationTradeStore)

  val tradeImporterFactory = new TradeImporterFactory(
    refinedAssignmentImporter, refinedFixationImporter
  )

  val enabledDesks: Set[Desk] = {
    props.EnabledDesks() match {
      case "" => Desk.all.toSet
      case "none" => Set.empty
      case names => names.split(",").toList.map(Desk.fromName).toSet
    }
  }

  val closedDesks = new ClosedDesks(broadcaster, starlingDB)

  val revalSnapshotDb = new RevalSnapshotDB(starlingDB)
  val limServer = new LIMServer(props.LIMHost(), props.LIMPort())

  val (fwdCurveAutoImport, marketDataStore) = {
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
      TrinityDiscountFactorCSV → new TrinityDiscountFactorCSVDataSource()
    )

    val mds = new DBMarketDataStore(starlingRichDB,
      marketDataSources,
      broadcaster)

    val fwdCurveAutoImport = new FwdCurveAutoImport(60, mds, marketDataSources.flatMap {
      case (k, f: FwdCurveDbMarketDataSource) => Some(k, f)
      case _ => None
    }, varSqlServerDB)
    (fwdCurveAutoImport, mds)
  }

  val userSettingsDatabase = new UserSettingsDatabase(starlingDB, broadcaster)

  val tradeImporters = Map[TradeSystem,TradeImporter](
    RefinedAssignmentTradeSystem → refinedAssignmentImporter,
    RefinedFixationTradeSystem → refinedFixationImporter)

  val tradeStores = new TradeStores(
    tradeImporters,
    closedDesks,
    eaiTradeStores, intradayTradesDB,
    refinedAssignmentTradeStore, refinedFixationTradeStore
  )

  val reportContextBuilder = new ReportContextBuilder(marketDataStore)
  val reportService = new ReportService(
    reportContextBuilder,
    tradeStores
  )

  val hostname = try {
    InetAddress.getLocalHost.getHostName
  } catch {
    case _ => "Unknown Host"
  }

  private val production = props.Production()
  private val serverName = props.ServerName()
  private val guiColour = if (production || props.UseProductionColour()) None else Some(PropsHelper.createColour(serverName))
  val version = Version(serverName, hostname, props.StarlingDatabase().url, production, guiColour)

  val trinityUploadCodeMapper = new TrinityUploadCodeMapper(trinityDB)
  val curveViewer = new CurveViewer(marketDataStore)
  val trinityUploader = new TrinityUploader(new FCLGenerator(trinityUploadCodeMapper, curveViewer), new XRTGenerator(marketDataStore), props)
  val scheduler = Scheduler.create(businessCalendars, marketDataStore, broadcaster, trinityUploader, props)

  val referenceData = new ReferenceData(businessCalendars, marketDataStore, strategyDB, scheduler, trinityUploadCodeMapper)

  val userReportsService = new UserReportsService(businessCalendars.UK, tradeStores, marketDataStore, userSettingsDatabase, reportService)

  val traders = new Traders(ldapUserLookup.user _)

  val starlingServer = new StarlingServerImpl(name, reportContextBuilder, reportService, marketDataStore,
    userSettingsDatabase, userReportsService, curveViewer, tradeStores, enabledDesks,
    version, referenceData, businessCalendars.UK, ldapUserLookup, eaiStarlingSqlServerDB, traders)

  val rmiPort = props.RmiPort()

  val users = new CopyOnWriteArraySet[User]

  val jmx = new StarlingJMX(users)

  val auth:ServerAuthHandler = props.UseAuth() match {
    case false => {
      Log.warn("Auth disabled")
      new ServerAuthHandler(new NullAuthHandler(Some(User.Dev)), users, ldapUserLookup,
        user => broadcaster.broadcast(UserLoggedIn(user)))
    }
    case true => {
      val kerberos = new ServerLogin(props.KerberosPassword())
      new ServerAuth(kerberos, ldapUserLookup, users, user => broadcaster.broadcast(UserLoggedIn(user))).handler
    }
  }

  val excelLoopReceiver = new ExcelLoopReceiver(ldapUserLookup, props.XLLoopPort(),
    new MarketDataHandler(broadcaster, starlingServer, marketDataStore),
    new TradeHandler(broadcaster, new ExcelTradeReader(strategyDB, traders), intradayTradesDB, traders),
    new SpecialSitsHandler(userReportsService, marketDataStore, mailSender),
    new ReportHandler(userReportsService),
    new DiagnosticHandler(starlingServer))

  val loopyXLReceiver = new LoopyXLReceiver(props.LoopyXLPort(), auth,
    new CurveHandler(curveViewer, marketDataStore))

  val latestTimestamp = if (forceGUICompatability) GUICode.latestTimestamp.toString else BouncyRMI.CodeVersionUndefined
  val rmiServer:BouncyRMIServer[StarlingServer] = new BouncyRMIServer(
    rmiPort,
    ThreadNamingProxy.proxy(starlingServer, classOf[StarlingServer]),
    auth, latestTimestamp, users,
    Set(classOf[UnrecognisedTradeIDException])
  )

  val eaiAutoImport = new EAIAutoImport(15, starlingRichDB, eaiStarlingRichSqlServerDB, strategyDB, eaiTradeStores, closedDesks, enabledDesks)

  val deltaCSVDir = props.DeltaCSVDir()

  val reportServlet = new ReportServlet("reports", userReportsService) //Don't add to main web server (as this has no authentication)

  val httpServer = locally {
    val externalURL = props.ExternalUrl()
    val externalHostname = props.ExternalHostname()
    val xlloopUrl = props.XLLoopUrl()
    val rmiPort = props.RmiPort()
    val classesServlet = new ClassesServlet("classes")
    val webStartServlet = new WebStartServlet("webstart", props.ServerName(), externalURL, "starling.gui.Launcher", List(externalHostname, rmiPort.toString), xlloopUrl)
    val cannedWebStartServlet = new WebStartServlet("cannedwebstart", props.ServerName(), externalURL, "starling.gui.CannedLauncher", List(), xlloopUrl)

    new HttpServer(props, "webstart" → webStartServlet, "cannedwebstart" → cannedWebStartServlet, "classes" → classesServlet)
  }

  val regressionServer = locally {
    val server = new JettyServer()
    val connector = new SocketConnector()
    connector.setHost("127.0.0.1")
    connector.setPort(props.RegressionPort())
    server.addConnector(connector)
    val rootContext = new Context(server, "/", Context.SESSIONS);
    rootContext.addServlet(new ServletHolder(reportServlet), "/reports/*")
    server
  }
}

object StarlingInit{
  lazy val devInstance = new StarlingInit(PropsHelper.defaultProps, true, false, false, false, false, false, false).start
}
object ServerHelper {
  val RunningFileName = "starling.running"

  def createRunningFile {
    val file = new File(RunningFileName)
    if (!file.createNewFile) throw new Exception("Couldn't create the " + RunningFileName + " file")
  }

  def deleteRunningFile {
    val file = new File(RunningFileName)
    if (file.exists) file.delete
  }
}

/**
 * The main entry point into Starling
 */
object Server extends OutputPIDToFile {
  def main(args: Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    ServerHelper.deleteRunningFile
    PropsHelper.writeDefaults
    run(PropsHelper.defaultProps)
    ServerHelper.createRunningFile
  }
  def run(props:Props) {
    new StarlingInit(props, true, true, true, startEAIAutoImportThread = true).start
  }
}

object Foo {
  def main(args: Array[String]) {
    val props = PropsHelper.defaultProps
    val db = DB(props.StarlingDatabase())

    val trinity = DB(props.TrinityDatabase())
    println(trinity.queryWithResult("select * from T_Commcode", Map()) { rs => rs.toString }.mkString("\n"))

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