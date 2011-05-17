package starling.endtoend

class GalenaPerformanceTests

/*
import starling.utils.StarlingTest
import org.testng.Assert._
import starling.endtoend.EndToEndTest._
import starling.utils.Stopwatch._
import org.testng.annotations._
import starling.daterange.{Timestamp, Day}
import starling.db.{CurveIdentifier, PricingGroup, GalenaTradeSystem, SnapshotID}
import starling.rmi.{ReportParameters, TradeSetList, StarlingServer}
import starling.reports.pivot.TradeStoreTradeSet
import starling.gui.api.ReportOptionsAvailable
import starling.pivot.{PivotFieldParams, Field, SomeSelection}
import starling.tradestore.TradePredicate

class GalenaPerformanceTests extends StarlingTest {
  val dayToUse = Day(2010, 5, 7)

  val gmaSnapshotID = SnapshotID(dayToUse, 4103, Timestamp.parse("11May2010 17:28:53.000"))
  val gmaPricingGroupToUse = "Galena/GMA (Full Curve)"

  val gmfSnapshotID = SnapshotID(dayToUse, 4103, Timestamp.parse("11May2010 17:28:53.000"))
  val gmfPricingGroupToUse = "Galena/GMF (Full Curve)"

  val oilSnapshotID = SnapshotID(dayToUse, 4103, Timestamp.parse("11May2010 17:28:53.000"))
  val oilPricingGroupToUse = "Galena/Oil (Live)"

  var starlingServer:StarlingServer = null
  var reportOptions:ReportOptionsAvailable = null

  var pricingGroupInfo:Map[PricingGroup, List[SnapshotID]] = null

  var gmaPricingGroup:PricingGroup = null
  var gmaCurveIdentifier:CurveIdentifier = null

  var gmfPricingGroup:PricingGroup = null
  var gmfCurveIdentifier:CurveIdentifier = null

  var oilPricingGroup:PricingGroup = null
  var oilCurveIdentifier:CurveIdentifier = null

  @BeforeClass
  def setUp {
    createTestProps
    startStarling
    starlingServer = ServiceResolver.resolveService("StarlingServer").asInstanceOf[StarlingServer]
    reportOptions = StarlingServer.reportOptionsAvailable
    pricingGroupInfo = StarlingServer.pricingGroupData(Some(GalenaTradeSystem))
    gmaPricingGroup = pricingGroupInfo.keySet.filter(_.label == gmaPricingGroupToUse).iterator.next
    gmaCurveIdentifier = CurveIdentifier(gmaPricingGroup, gmaSnapshotID)
    gmfPricingGroup = pricingGroupInfo.keySet.filter(_.label == gmfPricingGroupToUse).iterator.next
    gmfCurveIdentifier = CurveIdentifier(gmfPricingGroup, gmfSnapshotID)
    oilPricingGroup = pricingGroupInfo.keySet.filter(_.label == oilPricingGroupToUse).iterator.next
    oilCurveIdentifier = CurveIdentifier(oilPricingGroup, oilSnapshotID)
  }

  @AfterClass
  def tearDown {
    stopRMIConnection
    stopStarling
    removeTestProps
  }

  private def generateTradePredicate(reportingEntity:String) = TradePredicate(List((Field("In VAR"), new SomeSelection(Set("Included"))), (Field("Reporting Entity"), new SomeSelection(Set(reportingEntity)))))
  private def getGalenaTrades(reportingEntity:String) = TradeSetList(List(TradeStoreTradeSet(GalenaTradeSystem, generateTradePredicate(reportingEntity))))

  def getReportOptions = {
    starling.gui.api.ReportOptions(reportOptions.options, None, None)
  }

  @Test(groups = Array("performance"))
  def testAzurite {
    val trades = getGalenaTrades("Azurite")
    val reportParameters = ReportParameters(trades, gmaCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Azurite All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  @Test(groups = Array("performance"))
  def testGalena {
    val trades = getGalenaTrades("Galena")
    val reportParameters = ReportParameters(trades, gmaCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Galena All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  @Test(groups = Array("performance"))
  def testMalachite {
    val trades = getGalenaTrades("Malachite")
    val reportParameters = ReportParameters(trades, gmaCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Malachite All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  @Test(groups = Array("performance"))
  def testGMF {
    val trades = getGalenaTrades("Gmf")
    val reportParameters = ReportParameters(trades, gmfCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Gmf All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  @Test(groups = Array("performance"))
  def testGSA {
    val trades = getGalenaTrades("Gsa")
    val reportParameters = ReportParameters(trades, gmfCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Gsa All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  @Test(groups = Array("performance"))
  def testOil {
    val trades = getGalenaTrades("Oil")
    val reportParameters = ReportParameters(trades, oilCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Oil All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  private def generateErrorString(task:Task[_]) = {
    "Time taken to run " + task.name + " was " + milliToHumanString(task.timingInfo.timeTaken) +
              " but it needs to be under " + milliToHumanString(task.expectedTime.get)
  }
}*/
