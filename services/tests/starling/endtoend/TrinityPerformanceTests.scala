package starling.endtoend

class TrinityPerformanceTests

/*
import starling.utils.StarlingTest
import org.testng.Assert._
import starling.endtoend.EndToEndTest._
import starling.utils.Stopwatch._
import org.testng.annotations._
import starling.daterange.{Timestamp, Day}
import starling.db._
import starling.reports.pivot.TradeStoreTradeSet
import starling.pivot.{PivotFieldParams, Field, SomeSelection}
import starling.tradestore.TradePredicate
import starling.gui.api.{ReportParameters, TradeSetList, ReportOptionsAvailable}
import starling.rmi.StarlingServer

class TrinityPerformanceTests extends StarlingTest {
  val dayToUse = Day(2010, 5, 7)

  val freightSnapshotID = SnapshotID(dayToUse, 4103, Timestamp.parse("11May2010 17:28:53.000"))
  val freightPricingGroupToUse = "Trinity/Freight (Live)"

  val metalsSnapshotID = SnapshotID(dayToUse, 4103, Timestamp.parse("11May2010 17:28:53.000"))
  val metalsPricingGroupToUse = "Trinity/Metals (Live)"

  var starlingServer:StarlingServer = null
  var reportOptions:ReportOptionsAvailable = null

  var pricingGroupInfo:Map[PricingGroup, List[SnapshotID]] = null

  var freightPricingGroup:PricingGroup = null
  var freightCurveIdentifier:CurveIdentifier = null

  var metalsPricingGroup:PricingGroup = null
  var metalsCurveIdentifier:CurveIdentifier = null



  @BeforeClass
  def setUp {
    createTestProps
//    startStarling
    starlingServer = ServiceResolver.resolveService("StarlingServer").asInstanceOf[StarlingServer]
    reportOptions = StarlingServer.reportOptionsAvailable
    pricingGroupInfo = StarlingServer.pricingGroupData(Some(TrinityTradeSystem))
    freightPricingGroup = pricingGroupInfo.keySet.filter(_.label == freightPricingGroupToUse).iterator.next
    freightCurveIdentifier = CurveIdentifier(freightPricingGroup, freightSnapshotID)
    metalsPricingGroup = pricingGroupInfo.keySet.filter(_.label == metalsPricingGroupToUse).iterator.next
    metalsCurveIdentifier = CurveIdentifier(metalsPricingGroup, metalsSnapshotID)
  }

  @AfterClass
  def tearDown {
    stopRMIConnection
//    stopStarling
    removeTestProps
  }

  private def generateTradePredicate(reportingEntity:String) = TradePredicate(List((Field("In VAR"), new SomeSelection(Set("Included"))), (Field("Reporting Entity"), new SomeSelection(Set(reportingEntity)))))
  private def getTrinityTrades(reportingEntity:String) = TradeSetList(List(TradeStoreTradeSet(TrinityTradeSystem, generateTradePredicate(reportingEntity))))

  def getReportOptions  = {
    starling.gui.api.ReportOptions(reportOptions.options, None, None)
  }

  @Test(groups = Array("performance"))
  def testFreight {
    val trades = getTrinityTrades("Freight")
    val reportParameters = ReportParameters(trades, freightCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Freight All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  @Test(groups = Array("performance"))
  def testMetals {
    val trades = getTrinityTrades("Metals")
    val reportParameters = ReportParameters(trades, metalsCurveIdentifier, getReportOptions, new Timestamp)
    val task = Task("Metals All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  private def generateErrorString(task:Task[_]) = {
    "Time taken to run " + task.name + " was " + milliToHumanString(task.timingInfo.timeTaken) +
              " but it needs to be under " + milliToHumanString(task.expectedTime.get)
  }
}*/
