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
import io.Source
import collection.mutable.ArraySeq
import starling.curves.Environment._
import starling.curves._
import starling.utils.QuantityTestUtils._
import starling.utils.{Log, StringIO}
import starling.concurrent.MP._

class ExcelTradeReaderTests extends JonTestEnv {
  @AfterClass
  def tearDown {
    User.setLoggedOn(None)
  }

  @Test
  def aliasesShouldHaveNoErrors {
    ExcelInstrumentReader.marketAliases
  }

  val eAIStrategyDB = mock(classOf[EAIStrategyDB])
  when(eAIStrategyDB.pathFor(any(classOf[TreeID]))) thenReturn PivotTreePath("test [54418]")
  when(eAIStrategyDB.getStrategyFromDealId(any(classOf[TreeID]))) thenReturn Some(TreeID(54418))

  val jf = Some(User("jon.fox", "Jon Fox", None, Nil, "", "", ""))
  val sp = Some(User("seetal.patel", "Seetal Patel", None, Nil, "", "", ""))
  def currentUser() = jf.get
  val traders = new Traders({
    s => s match {
      case "jon.fox" => jf
      case "seetal.patel" => sp
      case _ => sp
    }})

  @Test
  def testRow {
    val row = Map("id" -> "1", "trader" -> "Jon Fox", "tradedFor" -> "Jaakko", "trade date" -> "01-Oct-10", "SIze" -> "-13", "unit" -> "lots",
      "market" -> "nymex wti", "instr" -> "future", "period" -> "dec-10", "price" -> "77.13", "counterparty" -> "ttf",
      "broker" -> "ice", "strategy" -> "54418", "comment" -> "a test", "clearing house" -> "", "entry date" -> "")


    val reader = new ExcelTradeReader(eAIStrategyDB, traders, currentUser)
    val trade = reader.allTrades(List(row), "some group").head
    val attr = IntradayTradeAttributes(Some(TreeID(54418)), TreeID(0), Some(TreeID(54418)), "Jon Fox", "Jaakko", "ice", "a test", "", "some group", Day(2010, 10, 1), "jon.fox")
    val tradeID = TradeID("some group-0001", IntradayTradeSystem)
    assertEquals(trade, Trade(tradeID, 01 Oct 2010, "ttf", attr, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List()))
  }

  @Test
  def testBigBlotter {
    val a = StringIO.lines("/starling/services/trade/SeetalBlotterTrades.csv").toList

    val header: Array[String] = a.head.split('\t')
    val rest = a.tail.map(_.split('\t').toSeq).asInstanceOf[List[Seq[Object]]]
    val reader = new ExcelTradeReader(eAIStrategyDB, traders, currentUser)
    val all: List[Trade] = Log.infoWithTime("all trades") {
      reader.allTrades(header, rest, "test")
    }

    val marketDayAndTime = Day(2011, 8, 1).endOfDay
    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(market, _, _) => Quantity(100, market.priceUOM)
          case IndexFixingKey(index, _) => Quantity(100, index.priceUOM)
          case MarketFixingKey(market, _, _) => Quantity(100, market.priceUOM)
        }

        def marketDay = marketDayAndTime
      }
    ).undiscounted

    Log.infoWithTime("Valuing blotter trades") {
      all.mpMap(trade => {
        val explained = trade.explain(env)
        val mtm = trade.assets(env).mtm(env, USD)
        assertQtyClose(explained, mtm, 1e-4, message = trade.toString)
      })
    }
  }
}