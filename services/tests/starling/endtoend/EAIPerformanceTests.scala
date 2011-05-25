package starling.endtoend

class EAIPerformanceTests

/*
import starling.utils.StarlingTest
import org.testng.Assert._
import starling.endtoend.EndToEndTest._
import starling.utils.Stopwatch._
import org.testng.annotations._
import starling.daterange.{Timestamp, Day}
import starling.rmi.{ReportParameters, TradeSetList, StarlingServer}
import starling.db._
import starling.reports.pivot.TradeStoreTradeSet
import starling.gui.api.ReportOptionsAvailable
import starling.pivot.{PivotFieldParams, Field, SomeSelection}
import starling.tradestore.TradePredicate

class EAIPerformanceTests extends StarlingTest {
  val dayToUse = Day(2010, 5, 7)

  val snapshotID = SnapshotID(dayToUse, 4103, Timestamp.parse("11May2010 17:28:53.000"))
  val pricingGroupToUse = "Trinity/Freight (Live)"

  var starlingServer:StarlingServer = null
  var reportOptions:ReportOptionsAvailable = null

  var pricingGroupInfo:Map[PricingGroup, List[SnapshotID]] = null

  var pricingGroup:PricingGroup = null
  var curveIdentifier:CurveIdentifier = null

  def getReportOptions = {
    starling.gui.api.ReportOptions(reportOptions.options, None, None)
  }

  @BeforeClass
  def setUp {
    createTestProps
    startStarling
    starlingServer = ServiceResolver.resolveService("StarlingServer").asInstanceOf[StarlingServer]
    reportOptions = StarlingServer.reportOptionsAvailable
    pricingGroupInfo = StarlingServer.pricingGroupData(Some(EAITradeSystem))
    pricingGroup = pricingGroupInfo.keySet.filter(_.label == pricingGroupToUse).iterator.next
    curveIdentifier = CurveIdentifier(pricingGroup, snapshotID)
  }

  @AfterClass
  def tearDown {
    stopRMIConnection
    stopStarling
    removeTestProps
  }

  private def generateTradePredicate(book:String) = TradePredicate(List((Field("Book"), new SomeSelection(Set(book)))))
  private def getEAITrades(book:String) = TradeSetList(List(TradeStoreTradeSet(EAITradeSystem, generateTradePredicate(book))))

  @Test(groups = Array("performance"))
  def testFreight {
    val trades = getEAITrades("London Derivatives Options")
    val reportParameters = ReportParameters(trades, curveIdentifier, getReportOptions, new Timestamp)
    val task = Task("London Derivatives Options All Reports", starlingServer.reportPivot(reportParameters, PivotFieldParams(true, None)), Some(5 * 1000))
    assertTrue(task.timingInfo.timeTaken < task.expectedTime.get, generateErrorString(task))
  }

  private def generateErrorString(task:Task[_]) = {
    "Time taken to run " + task.name + " was " + milliToHumanString(task.timingInfo.timeTaken) +
              " but it needs to be under " + milliToHumanString(task.expectedTime.get)
  }
}*/
