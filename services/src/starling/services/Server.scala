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


class StarlingInit( val props: Props,
                    dbMigration: Boolean = true,
                    startRMI: Boolean = true,
                    startHttp: Boolean = true,
                    startXLLoop: Boolean = true,
                    startStarlingJMX: Boolean = true,
                    forceGUICompatability: Boolean = true,
                    startEAIAutoImportThread: Boolean = true,
                    startRabbit: Boolean = true
                    ) {

  def stop = {
    if (startRabbit) {
      titanRabbitEventServices.stop
    }

    if (startXLLoop) {
      excelLoopReceiver.stop
      loopyXLReceiver.stop
    }
    if (startRMI) {
      rmiServerForTitan.stop()
    }
    if (startHttp) {
      httpServer.stop
      //httpEdmServiceServer.stop
      regressionServer.stop
    }
    scheduler.stop
    ConnectionParams.shutdown
  }

  def start = {
    locally {
      // Load all user settings, pivot layouts and user reports here to ensure we don't have any de-serialization issues. Do this before anyone can connect.
      userSettingsDatabase.readAll()
    }

    if (startRabbit) {
      titanRabbitEventServices.start
    }


    if (startXLLoop) {
      excelLoopReceiver.start
      loopyXLReceiver.start
    }

    if (startHttp) {
      httpServer.run
      //httpEdmServiceServer.run
      regressionServer.start
    }

    if (startStarlingJMX) {
      jmx.start
    }

    if (startRMI) {
      rmiServerForTitan.start
      Log.info("Titan RMI started on port " + props.StarlingServiceRmiPort())
      rmiServerForGUI.start
      Log.info("GUI RMI started on port " + rmiPort)
    }

    if (startEAIAutoImportThread) {
      eaiAutoImport.schedule
      fwdCurveAutoImport.schedule
    }

    scheduler.start

    this
  }

  val runtime = Runtime.getRuntime
  Log.info("StarlingInit: maxMemory %sMB" % (runtime.maxMemory / 1024 / 1024))

  val name = props.ServerName()
  val ldapUserLookup = new LdapUserLookup with BouncyLdapUserLookup[User]

  // basic DB connections
  val starlingDB = DB(props.StarlingDatabase())
//  starlingDB.setReadCommittedSnapshot
  val trinityDB = DB(props.TrinityDatabase())
  val galenaDB = DB(props.GalenaDatabase())
  val varSqlServerDB = DB(props.VarSqlServer())
  val eaiSqlServerDB = DB(props.EAIReplica())
  val eaiStarlingSqlServerDB = DB(props.EAIStarlingSqlServer())

  // Things that need access to a DB like portfolios, holidays, markets, indexes...
  val holidayTables = new Holidays(eaiSqlServerDB)
  HolidayTablesFactory.registerHolidayTablesImpl(holidayTables)
  val businessCalendars = new BusinessCalendars(holidayTables)
  val expiryRules = new FuturesExpiryRulesImpl(eaiSqlServerDB, businessCalendars)
  FuturesExpiryRuleFactory.registerRulesImpl(expiryRules)
  val marketLookup = new StarlingMarketLookup(starlingDB, businessCalendars, expiryRules)
  MarketProvider.registerImpl(marketLookup)
  val richResultSetRowFactory = new RichResultSetRowFactory

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

  val trinityService = TrinityService(ResteasyServiceApi(props.TrinityServiceUrl()))

  val titanRabbitEventServices = new DefaultTitanRabbitEventServices(props)

  val broadcaster = new CompositeBroadcaster(
      true                                      → new RMIBroadcaster(rmiServerForGUI),
      props.rabbitHostSet                       → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
      props.EnableVerificationEmails()          → new EmailBroadcaster(mailSender),
      (startRabbit && props.titanRabbitHostSet) → TitanRabbitIdBroadcaster(titanRabbitEventServices.rabbitEventPublisher))

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
      TrinityDiscountFactorCSV → new TrinityDiscountFactorCSVDataSource()//,
      //Neptune -> new NeptuneBenchmarksMarketDataSource(neptuneRichDB) I don't want this persisted yet as it is likely to change
    )

    val mds = new DBMarketDataStore(starlingRichDB,
      marketDataSources,
      broadcaster)

    val fwdCurveAutoImport = new FwdCurveAutoImport(60*15, mds, marketDataSources.flatMap {
      case (k, f: FwdCurveDbMarketDataSource) => Some(k)
      case _ => None
    }.toSet, businessCalendars.US_UK)
    (fwdCurveAutoImport, mds)
  }

  val userSettingsDatabase = new UserSettingsDatabase(starlingDB, broadcaster)

  if (dbMigration) {
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

  val titanTradeCache = new DefaultTitanTradeCache(props)
  val titanServices = new DefaultTitanServices(props)
  val logisticsServices = new DefaultTitanLogisticsServices(props, Some(titanServices))
  val titanInventoryCache = new DefaultTitanLogisticsInventoryCache(props)
  val valuationService = new ValuationService(new DefaultEnvironmentProvider(marketDataStore), titanTradeCache, titanServices, logisticsServices, titanRabbitEventServices, titanInventoryCache)
  val marketDataService = new MarketDataService(marketDataStore)

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

  val curveViewer = new CurveViewer(marketDataStore)
  val trinityUploader = new TrinityUploader(new FCLGenerator(businessCalendars, curveViewer),
    new XRTGenerator(marketDataStore), trinityService, props)
  val scheduler = Scheduler.create(businessCalendars, marketDataStore, broadcaster, trinityUploader, props)

  val referenceData = new ReferenceData(businessCalendars, marketDataStore, strategyDB, scheduler)

  val userReportsService = new UserReportsService(businessCalendars.UK, tradeStores, marketDataStore, userSettingsDatabase, reportService)

  val traders = new Traders(ldapUserLookup.user _)

  val starlingServer = new StarlingServerImpl(name, reportContextBuilder, reportService, marketDataStore,
    userSettingsDatabase, userReportsService, curveViewer, tradeStores, enabledDesks,
    version, referenceData, businessCalendars.UK, ldapUserLookup, eaiStarlingSqlServerDB, traders)

  val rmiPort = props.RmiPort()

  val users = new CopyOnWriteArraySet[User]

  val jmx = new StarlingJMX(users, scheduler)

  val auth:ServerAuthHandler[User] = props.UseAuth() match {
    case false => {
      Log.warn("Auth disabled")
      new ServerAuthHandler[User](new NullAuthHandler(Some(User.Dev)), users, ldapUserLookup,
        user => broadcaster.broadcast(UserLoggedIn(user)), ChannelLoggedIn)
    }
    case true => {
      val kerberos = new ServerLogin(props.KerberosPassword())
      new ServerAuth(kerberos, ldapUserLookup, users, user => broadcaster.broadcast(UserLoggedIn(user))).handler
    }
  }

  val excelLoopReceiver = new ExcelLoopReceiver(ldapUserLookup, props.XLLoopPort(),
    new MarketDataHandler(broadcaster, starlingServer, marketDataStore),
    new TradeHandler(broadcaster, new ExcelTradeReader(strategyDB, traders), intradayTradesDB, traders),
    new ReportHandler(userReportsService),
    new DiagnosticHandler(starlingServer))

  val loopyXLReceiver = new LoopyXLReceiver(props.LoopyXLPort(), auth,
    new CurveHandler(curveViewer, marketDataStore))

   val latestTimestamp = if (forceGUICompatability) 
     GUICode.latestTimestamp.toString 
   else 
     BouncyRMI.CodeVersionUndefined

  val rmiServerForGUI:BouncyRMIServer[User] = new BouncyRMIServer(
    rmiPort,
    auth, latestTimestamp, users,
    Set(classOf[UnrecognisedTradeIDException]),
    ChannelLoggedIn,
    ThreadNamingProxy.proxy(starlingServer, classOf[StarlingServer])
  )

  /**
   * start up public services for Titan components
   */
  println("Titan service port " + props.StarlingServiceRmiPort())
  val nullHandler = new ServerAuthHandler[User](new NullAuthHandler(Some(User.Dev)), users, ldapUserLookup,
        user => broadcaster.broadcast(UserLoggedIn(user)), ChannelLoggedIn)
  val rmiServerForTitan : BouncyRMIServer[User] = new BouncyRMIServer(
    props.StarlingServiceRmiPort(),
    nullHandler, BouncyRMI.CodeVersionUndefined, users,
    Set(classOf[TradeManagementCacheNotReady], classOf[IllegalArgumentException]),
    ChannelLoggedIn,
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
    val webStartServlet = new WebStartServlet("webstart", props.ServerName(), externalURL, "starling.gui.Launcher",
      List(externalHostname, rmiPort.toString), xlloopUrl)
    val cannedWebStartServlet = new WebStartServlet("cannedwebstart", props.ServerName(), externalURL,
      "starling.gui.CannedLauncher", List(), xlloopUrl)

    new HttpServer(props,
      "webstart"       → webStartServlet,
      "cannedwebstart" → cannedWebStartServlet)
  }

  Log.info("StarlingInit: EDM service port %d, external url = '%s', server name = '%s'".format(props.HttpEdmServicePort(), props.EdmExternalUrl(), props.ServerName()))
  
  lazy val httpEdmServiceServer = {
    val webXmlUrl = this.getClass.getResource("../../webapp/WEB-INF/web.xml")

    new HttpServer(
      props.HttpEdmServicePort(),
      props.EdmExternalUrl(),
      props.ServerName(),
      Some(webXmlUrl.toExternalForm()),
      Nil)
  }

  lazy val regressionServer = new RegressionServer(props.RegressionPort(), reportServlet)
}

object StarlingInit{
  lazy val devInstance = new StarlingInit(PropsHelper.defaultProps, true, false, false, false, false, false, false, false).start
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
    run(PropsHelper.defaultProps, args)
    ServerHelper.createRunningFile
  }

  var server: StarlingInit = null

  def run(props: Props, args: Array[String] = Array[String]()) {
    Log.infoWithTime("Launching starling server") {

      server = new StarlingInit(props, true, true, true, startEAIAutoImportThread = props.ImportsBookClosesFromEAI(),
        startRabbit = props.RabbitEnabled())
      server.start
    }
  }
}

case class FirstKey(time:ObservationTimeOfDay, marketDataSet:MarketDataSet, key:MarketDataKey)
case class SecondKey(key:SortedMap[Field,Any])

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
        val key = rs.getObject[MarketDataKey]("marketDataKey")
        id -> FirstKey(time, marketDataSet, key)
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

object ChannelLoggedIn extends LoggedIn[User] {
  private val loggedIn = new ChannelLocal[User]()

  def get(channel: Channel): User = loggedIn.get(channel)
  def set(channel: Channel, user: User): User = loggedIn.set(channel, user)
  def remove(channel: Channel): User = loggedIn.remove(channel)
  def setLoggedOn(user: Option[User]) {
    User.setLoggedOn(user)
  }
}

