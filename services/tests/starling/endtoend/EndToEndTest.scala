package starling.endtoend

import starling.utils.StarlingTest
import starling.utils.Stopwatch._
import collection.mutable.ListBuffer
import starling.services.ServerHelper
import starling.utils.sql.ConnectionParams
import java.io._
import starling.db._
import starling.utils.DatabaseUtils
import starling.pivot.{PivotFieldParams, SomeSelection, Field}
import starling.tradestore.TradePredicate
import starling.daterange.{Timestamp, Day}
import starling.rmi.StarlingServer
import starling.gui.api.ReportParameters
import starling.bouncyrmi.{BouncyRMI, BouncyRMIClient}
import starling.auth.Client
import starling.props.{PropsHelper, Props}

object EndToEndTest {
  val TestPropsName = "test_props.conf"
  val TestDatabase = "starling_test"
  val TestBackupName = "starling_test"
//  val TestDatabase = "starling_corina2"
//  val TestDatabase = "starling_NickD1"
  val TestDatabaseURL = "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/" + TestDatabase + ";instance=DB08"
  val TestDatabaseURLForCopy = "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com;instance=DB08"
  val TestDatabaseUser = "starling"
  val TestDatabasePass = "ng1lr4ts123!Y^%&$"
  val TestDatabaseString = "StarlingDatabase = " + TestDatabaseURL + " " + TestDatabaseUser + " " + TestDatabasePass
  val tasks = new ListBuffer[Task[_]]

  def copyDatabase {
    DatabaseUtils.refreshDatabase(TestDatabase, TestDatabaseURLForCopy, TestDatabaseUser, TestDatabasePass)
  }

  def backupDatabase {
    DatabaseUtils.backupDatabase(TestBackupName, TestDatabaseURLForCopy, TestDatabaseUser, TestDatabasePass)
  }

  private var UseTestProps = false

  def createTestProps {
    UseTestProps = true
    removeTestProps
    val testPropsFile = new File(TestPropsName)
    val writer = new PrintWriter(new FileWriter(testPropsFile))
    writer.println(TestDatabaseString)
    writer.close
  }

  def startStarling {
    // Remove the running file if there is one.
    val runningFile = new File(ServerHelper.RunningFileName)
    if (runningFile.exists) runningFile.delete

    // Start Starling.
    val startStarling = if (UseTestProps) {
      new ProcessBuilder("bin/start.sh", "-Dprops.location=" + TestPropsName).start
    } else {
      new ProcessBuilder("bin/start.sh").start
    }
    val bufferedReader = new BufferedReader(new InputStreamReader(startStarling.getInputStream))
    val errorBufferedReader = new BufferedReader(new InputStreamReader(startStarling.getErrorStream))
    var line = ""
    var errorLine = ""
    var cont = true
    while (cont) {
      line = bufferedReader.readLine
      errorLine = errorBufferedReader.readLine
      if (line != null) {
        println("Output: " + line)
      }
      if (errorLine != null) {
        println("Error: " + errorLine)
      }
      if ((line == null) && (errorLine == null)) {
        cont = false
      }
    }
    bufferedReader.close
    errorBufferedReader.close

    // Wait until starling is running... i.e. done all conversions and is ready to accept connections.
    var starlingRunning = false    
    while (!starlingRunning) {
      val file = new File(ServerHelper.RunningFileName)
      if (file.exists) {
        starlingRunning = true
      } else {
        Thread.sleep(1000)
      }
    }
  }

  def stopStarling {
    val stopStarling = new ProcessBuilder("bin/stop.sh").start
    val bufferedReader = new BufferedReader(new InputStreamReader(stopStarling.getInputStream))
    val errorBufferedReader = new BufferedReader(new InputStreamReader(stopStarling.getErrorStream))
    var line = ""
    var errorLine = ""
    var cont = true
    while (cont) {
      line = bufferedReader.readLine
      errorLine = errorBufferedReader.readLine
      if (line != null) {
        println("Output: " + line)
      }
      if (errorLine != null) {
        println("Error: " + errorLine)
      }
      if ((line == null) && (errorLine == null)) {
        cont = false
      }
    }
    bufferedReader.close
    errorBufferedReader.close
  }

  def removeTestProps {
    val testPropsFile = new File(TestPropsName)
    if (testPropsFile.exists) {
      testPropsFile.delete
    }
  }

  val clientAuth = new starling.bouncyrmi.Client {
    def ticket = null
  }
  val props = PropsHelper.defaultProps
  lazy val client = new BouncyRMIClient(props.ExternalHostname.value, props.RmiPort.value.toInt, clientAuth, overriddenUser = None)
  lazy val StarlingServer:StarlingServer = {
    client.start
    client.proxy(classOf[StarlingServer])
  }

  def stopRMIConnection {
    client.stop
  }

  /*def importTrades(tradeSystem:TradeSystem) {
    StarlingServer.importAllTrades(tradeSystem)
  }*/

//  lazy val GalenaPricingGroupInfo = StarlingServer.pricingGroupData(Some(GalenaTradeSystem))
//  lazy val TrinityPricingGroupInfo = StarlingServer.pricingGroupData(Some(TrinityTradeSystem))
//  lazy val EAIPricingGroupInfo = StarlingServer.pricingGroupData(Some(EAITradeSystem))

  val DayToUse = Day.today.previousWeekday

  // TODO [07 May 2010] put this back once it is working properly.
//  val GalenaMetalsPricingGroupToUse = "Galena/Live"
//  val GalenaMetalsPricingGroupToUse = "Galena/GMA (Full Curve)"
//  lazy val GalenaMetalsPricingGroup = GalenaPricingGroupInfo.keySet.filter(_.label == GalenaMetalsPricingGroupToUse).iterator.next
//  lazy val GalenaMetalsSnapshot = StarlingServer.snapshot(GalenaMetalsPricingGroup, DayToUse)
//  val GalenaOilPricingGroupToUse = "Galena/Oil (Live)"
//  lazy val GalenaOilPricingGroup = GalenaPricingGroupInfo.keySet.filter(_.label == GalenaOilPricingGroupToUse).iterator.next
//  lazy val GalenaOilSnapshot = StarlingServer.snapshot(GalenaOilPricingGroup, DayToUse)
//
//  val TrinityFreightPricingGroupToUse = "Trinity/Freight (Live)"
//  lazy val TrinityFreightPricingGroup = TrinityPricingGroupInfo.keySet.filter(_.label == TrinityFreightPricingGroupToUse).iterator.next
//  lazy val TrinityFreightSnapshot = StarlingServer.snapshot(TrinityFreightPricingGroup, DayToUse)
//  val TrinityMetalsPricingGroupToUse = "Trinity/Metals (Live)"
//  lazy val TrinityMetalsPricingGroup = TrinityPricingGroupInfo.keySet.filter(_.label == TrinityMetalsPricingGroupToUse).iterator.next
//  lazy val TrinityMetalsSnapshot = StarlingServer.snapshot(TrinityMetalsPricingGroup, DayToUse)
//
//  val EAIPricingGroupToUse = "FwdCurveApp/SYSTEM"
//  lazy val EAIPricingGroup = EAIPricingGroupInfo.keySet.filter(_.label == EAIPricingGroupToUse).iterator.next
//  lazy val EAISnapshot = StarlingServer.snapshot(EAIPricingGroup, DayToUse)

  /*lazy val ReportOptions = StarlingServer.reportOptionsAvailable

  def reportOptions = {
    starling.gui.api.ReportOptions(ReportOptions.options, None, None)
  }

  lazy val GalenaMetalsPivotData = {
    // TODO [07 May 2010] this isn't using all the metals
    val tradePredicate = TradePredicate(List((Field("In VAR"), new SomeSelection(Set("Included"))), (Field("Reporting Entity"), new SomeSelection(Set("Azurite")))))
    val trades = TradeSetList(List(TradeStoreTradeSet(GalenaTradeSystem, tradePredicate)))
    val curveIdentifier = CurveIdentifier(GalenaMetalsPricingGroup, GalenaMetalsSnapshot)
    val reportParameters = ReportParameters(trades, curveIdentifier, reportOptions, new Timestamp)
    StarlingServer.reportPivot(reportParameters, PivotFieldParams(true, None))
  }

  lazy val GalenaOilPivotData = {
    val tradePredicate = TradePredicate(List((Field("In VAR"), new SomeSelection(Set("Included"))), (Field("Reporting Entity"), new SomeSelection(Set("Oil")))))
    val trades = TradeSetList(List(TradeStoreTradeSet(GalenaTradeSystem, tradePredicate)))
    val curveIdentifier = CurveIdentifier(GalenaOilPricingGroup, GalenaOilSnapshot)
    val reportParameters = ReportParameters(trades, curveIdentifier, reportOptions, new Timestamp)
    StarlingServer.reportPivot(reportParameters, PivotFieldParams(true, None))
  }

  lazy val TrinityFreightPivotData = {
    val tradePredicate = TradePredicate(List((Field("In VAR"), new SomeSelection(Set("Included"))), (Field("Reporting Entity"), new SomeSelection(Set("Freight")))))
    val trades = TradeSetList(List(TradeStoreTradeSet(TrinityTradeSystem, tradePredicate)))
    val curveIdentifier = CurveIdentifier(TrinityFreightPricingGroup, TrinityFreightSnapshot)
    val reportParameters = ReportParameters(trades, curveIdentifier, reportOptions, new Timestamp)
    StarlingServer.reportPivot(reportParameters, PivotFieldParams(true, None))
  }

  lazy val TrinityMetalsPivotData = {
    val tradePredicate = TradePredicate(List((Field("In VAR"), new SomeSelection(Set("Included"))), (Field("Reporting Entity"), new SomeSelection(Set("Metals")))))
    val trades = TradeSetList(List(TradeStoreTradeSet(TrinityTradeSystem, tradePredicate)))
    val curveIdentifier = CurveIdentifier(TrinityMetalsPricingGroup, TrinityMetalsSnapshot)
    val reportParameters = ReportParameters(trades, curveIdentifier, reportOptions, new Timestamp)
    StarlingServer.reportPivot(reportParameters, PivotFieldParams(true, None))
  }

  lazy val EAIPivotData = {
    val tradePredicate = TradePredicate(List((Field("Book"), new SomeSelection(Set("London Derivatives Options")))))
    val trades = TradeSetList(List(TradeStoreTradeSet(TrinityTradeSystem, tradePredicate)))
    val curveIdentifier = CurveIdentifier(TrinityMetalsPricingGroup, TrinityMetalsSnapshot)
    val reportParameters = ReportParameters(trades, curveIdentifier, reportOptions, new Timestamp)
    StarlingServer.reportPivot(reportParameters, PivotFieldParams(true, None))
  }

  def runTrinity {
    tasks += ImportTrinityTradesTask
    tasks += FetchTrinityPricingGroupInfoTask
    tasks += GenerateTrinityFreightSnapshotTask
    tasks += GenerateTrinityMetalsSnapshotTask
    tasks += RunTrinityFreightReportTask
    tasks += RunTrinityMetalsReportTask
  }

  def runGalena {
    tasks += ImportGalenaTradesTask
    tasks += FetchGalenaPricingGroupInfoTask
    tasks += GenerateGalenaMetalsSnapshotTask
    tasks += GenerateGalenaOilSnapshotTask
    tasks += RunGalenaMetalsReportTask
    tasks += RunGalenaOilReportTask
  }

  def runEAI {
    tasks += ImportEAITradesTask
    tasks += FetchEAIPricingGroupInfoTask
    tasks += GenerateEAISnapshotTask
    tasks += RunEAIReportTask
  }*/

  lazy val BackupDatabaseTask = Task("Backup Database", backupDatabase)
  lazy val CopyDatabaseTask = Task("Copy Database", copyDatabase)
  lazy val CreateTestPropsTask = Task("Create Test Props", createTestProps)
  lazy val StartStarlingAndConvertTask = Task("Start Starling and Convert", startStarling)
  lazy val StartRMIConnectionTask = Task("Start RMI Connection", StarlingServer)
//  lazy val FetchReportOptionsTask = Task("Fetch Report Options", ReportOptions)

  /*lazy val RunTrinityTask = Task("Run Trinity", runTrinity)
  lazy val ImportTrinityTradesTask = Task("Import Trinity Trades", importTrades(TrinityTradeSystem))
  lazy val FetchTrinityPricingGroupInfoTask = Task("Fetch Trinity Pricing Group Info", TrinityPricingGroupInfo)
  lazy val GenerateTrinityFreightSnapshotTask = Task("Generate Trinity Freight Snapshot", TrinityFreightSnapshot)
  lazy val GenerateTrinityMetalsSnapshotTask = Task("Generate Trinity Metals Snapshot", TrinityMetalsSnapshot)
  lazy val RunTrinityFreightReportTask = Task("Run Trinity Freight Report", TrinityFreightPivotData)
  lazy val RunTrinityMetalsReportTask = Task("Run Trinity Metals Report", TrinityMetalsPivotData)

  lazy val RunGalenaTask = Task("Run Galena", runGalena)
  lazy val ImportGalenaTradesTask = Task("Import Galena Trades", importTrades(GalenaTradeSystem))
  lazy val FetchGalenaPricingGroupInfoTask = Task("Fetch Galena Pricing Group Info", GalenaPricingGroupInfo)
  lazy val GenerateGalenaMetalsSnapshotTask = Task("Generate Galena Metals Snapshot", GalenaMetalsSnapshot)
  lazy val GenerateGalenaOilSnapshotTask = Task("Generate Galena Oil Snapshot", GalenaOilSnapshot)
  lazy val RunGalenaMetalsReportTask = Task("Run Galena Metals Report", GalenaMetalsPivotData, Some(5 * 1000))
  lazy val RunGalenaOilReportTask = Task("Run Galena Oil Report", GalenaOilPivotData)

  lazy val RunEAITask = Task("Run EAI", runEAI)
  lazy val ImportEAITradesTask = Task("Import EAI Trades", importTrades(EAITradeSystem))
  lazy val FetchEAIPricingGroupInfoTask = Task("Fetch EAI Pricing Group Info", EAIPricingGroupInfo)
  lazy val GenerateEAISnapshotTask = Task("Generate EAI Snapshot", EAISnapshot)
  lazy val RunEAIReportTask = Task("Run EAI Report", EAIPivotData)*/

  lazy val StopRMIConnectionTask = Task("Stop RMI Connection", stopRMIConnection)
  lazy val StopStarlingTask = Task("Stop Starling", stopStarling)
  lazy val RemoveTestPropsTask = Task("Remove Test Props", removeTestProps)
    
  def main(args: Array[String]) {
    val today = Day.today
    tasks += CopyDatabaseTask
    tasks += CreateTestPropsTask
    tasks += StartStarlingAndConvertTask
   /* tasks += StartRMIConnectionTask
    tasks += FetchReportOptionsTask

    tasks += RunTrinityTask
    tasks += RunGalenaTask
    tasks += RunEAITask
    
    tasks += StopRMIConnectionTask*/
    tasks += StopStarlingTask
    tasks += RemoveTestPropsTask
    tasks += BackupDatabaseTask

    println("")
    println("Report for " + today)
    println("")
//    tasks.foreach(task => println(task.outputDetails))
    // Have to call exit explicitly as JRemoting doesn't want to seem to let go. 
    System.exit(0)
  }
}

class Task[T](val name:String, task: =>Unit, val expectedTime:Option[Long]) {
  println(name)
  private val timeWithInfoResult = timeWithInfo(task)
  val timingInfo = timeWithInfoResult._1
  val result = timeWithInfoResult._2
  def outputDetails = {
    val nameLength = 50
    val nameDiff = nameLength - name.length
    val nameExtra = " " * nameDiff
    def lengthenTime(time:Long, format:Long => String) = {
      val timeLength = 30
      val timeText = format(time)
      val timeDiff = timeLength - timeText.length
      (timeText + (" " * timeDiff))
    }
    (name + nameExtra + "Started: " + lengthenTime(timingInfo.startTime, milliToTimeString) + "Ended: " + lengthenTime(timingInfo.endTime, milliToTimeString) + "Time Taken: " + lengthenTime(timingInfo.timeTaken, milliToHumanString))
  }
}
object Task {
  def apply(name:String, task: =>Unit, expectedTime:Option[Long] = None) = new Task(name, task, expectedTime)
}
