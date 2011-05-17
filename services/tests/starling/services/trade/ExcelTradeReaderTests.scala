package starling.services.trade

import instrumentreaders.ExcelInstrumentReader
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.RichQuantity._
import starling.quantity.UOM._
import starling.utils.ScalaTestUtils._
import starling.market.{FuturesFrontPeriodIndex, JonTestEnv, Market}
import starling.market.Market._
import starling.daterange.Day._
import starling.instrument._
import starling.models.{Put, Call, American}
import starling.db.IntradayTradeSystem
import starling.daterange._
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.auth.User
import starling.eai.{Traders, TreeID, Tree, EAIStrategyDB}
import starling.trade.{Trade, TradeID}
import starling.pivot.PivotTreePath
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.testng.annotations.{AfterClass, DataProvider, Test}

class ExcelTradeReaderTests extends JonTestEnv {
  @AfterClass
  def tearDown {
    User.setLoggedOn(None)
  }

  @Test
  def aliasesShouldHaveNoErrors {
    ExcelInstrumentReader.marketAliases
  }

  @Test
  def testRow {
    val jf = Some(User("jon.fox", "Jon Fox", None, Nil, "", "", ""))
    val traders = new Traders({s => jf})
    User.setLoggedOn(jf)

    val row = Map("id" -> "1", "trader" -> "Jon Fox", "tradedFor" -> "Jaakko", "trade date" -> "01-Oct-10", "SIze" -> "-13", "unit" -> "lots",
      "market" -> "nymex wti", "instr" -> "future", "period" -> "dec-10", "price" -> "77.13", "counterparty" -> "ttf",
      "broker" -> "ice", "strategy" -> "54418", "comment" -> "a test", "clearing house" -> "", "entry date" -> "")

    val eAIStrategyDB = mock(classOf[EAIStrategyDB])
    when(eAIStrategyDB.pathFor(any(classOf[TreeID]))) thenReturn PivotTreePath("test [54418]")
    when(eAIStrategyDB.getStrategyFromDealId(any(classOf[TreeID]))) thenReturn Some(TreeID(54418))

    val reader = new ExcelTradeReader(eAIStrategyDB, traders)
    val trade = reader.allTrades(List(row), "some group").head
    val attr = IntradayTradeAttributes(Some(TreeID(54418)), TreeID(0), Some(TreeID(54418)), "Jon Fox", "Jaakko", "ice", "a test", "", "some group", Day(2010, 10, 1), "jon.fox")
    val tradeID = TradeID("some group-0001", IntradayTradeSystem)
    assertEquals(trade, Trade(tradeID, 01 Oct 2010, "ttf", attr, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List()))
  }

}