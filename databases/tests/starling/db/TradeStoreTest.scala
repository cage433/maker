package starling.db

import starling.utils.StarlingTest
import starling.market._
import starling.instrument._
import org.testng.Assert._
import org.springframework.jdbc.datasource.SingleConnectionDataSource
import org.mockito.Mockito.mock
import org.mockito.Mockito._
import starling.richdb.{RichDB, RichResultSetRowFactory}
import starling.pivot.Field._
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.market._
import starling.quantity.{UOM, Quantity}
import starling.models.{European, Call}
import swing.event.Event
import starling.utils.Broadcaster
import starling.daterange._
import starling.trade.{TradeID, Trade, TradeSystem}
import starling.tradestore.eai.{EAITradeStore, EAITradeAttributes}
import starling.pivot.{PivotTreePath, SomeSelection, Field}
import starling.eai.{Book, EAIStrategyDB, TreeID}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.testng.annotations.{AfterTest, BeforeTest, Test}
import java.sql.Connection

class EAITradeStoreTest extends TestMarketSpec {

  lazy val marketDataStore = new DBMarketDataStore(db, Map(), Broadcaster.Null)

  var db : RichDB = _
  var connection : Connection = _

  @BeforeTest
  def initialise {
    connection = DBTest.getConnection("jdbc:h2:mem:EAITradeStoreTest;create=true")
    val ds = new SingleConnectionDataSource(connection, true)
    db = new TestDB(ds, new RichResultSetRowFactory)
    db.inTransaction{
      writer => {
        writer.update(create_table)
      }
    }
  }

  @AfterTest
  def tearDown() {
    connection.close()
  }

  @Test
  def testStoreTrades {
    val broadcaster = new Broadcaster() {
      def broadcast(event: Event) = {}
    }

    val eAIStrategyDB = mock(classOf[EAIStrategyDB])
    when(eAIStrategyDB.pathFor(any(classOf[TreeID]))) thenReturn PivotTreePath("test [54418]")
    when(eAIStrategyDB.getStrategyFromDealId(any(classOf[TreeID]))) thenReturn Some(TreeID(54418))

    val store = new EAITradeStore(db, broadcaster, eAIStrategyDB, Book(2))

    val attr = EAITradeAttributes(TreeID(1), TreeID(2), TreeID(3), "trader", "tradedfor", "broker", "clearinghouse")

    val trade1a = Trade(TradeID(1, EAITradeSystem), Day.today, "cp", attr, ErrorInstrument("error1"))
    val trade2a = Trade(TradeID(2, EAITradeSystem), Day.today, "cp", attr, CommoditySwap.sample)
    val trade3a = Trade(TradeID(3, EAITradeSystem), Day.today, "cp", attr, Future.sample)

    val tradesA = List(trade1a, trade2a, trade3a)
    val storedA = store.storeTrades((trade) => true, tradesA, new Timestamp)

    assertEquals(storedA.inserted, 3)
    assertEquals(storedA.updated, 0)
    assertEquals(storedA.deleted, 0)
    assertEquals(storedA.changed, true)

    assertEquals(store.readLatestVersionOfAllTrades.size, 3)

    val trade1b = trade1a.copy(attributes = attr.copy(strategyID = TreeID(66)))
    val trade2b = trade2a.copy()
    val trade3b = trade3a.copy(tradeable = ErrorInstrument("error3"))

    val tradesB = List(trade1b, trade2b, trade3b)
    val storedB = store.storeTrades((trade) => true, tradesB, new Timestamp)

    assertEquals(storedB.inserted, 0)
    assertEquals(storedB.updated, 2)
    assertEquals(storedB.deleted, 0)
    assertEquals(storedB.changed, true)

    val trade2c = trade2b.copy(tradeable = CommoditySwap.sample.copy(_volume = Quantity(12.123, MT)))
    val trade3c = trade3b.copy()

    val tradesC = List(trade2c, trade3c)
    val storedC = store.storeTrades((trade) => true, tradesC, new Timestamp)

    assertEquals(storedC.inserted, 0)
    assertEquals(storedC.updated, 1)
    assertEquals(storedC.deleted, 1)
    assertEquals(storedC.changed, true)

    val storedD = store.storeTrades((trade) => true, tradesC, new Timestamp)

    assertEquals(storedD.inserted, 0)
    assertEquals(storedD.updated, 0)
    assertEquals(storedD.deleted, 0)
    assertEquals(storedD.changed, false)
  }


  val create_table = """
    CREATE TABLE EAITrade
    (
	id int GENERATED By DEFAULT AS IDENTITY (START WITH 1, INCREMENT BY 1) NOT NULL,
	tradeID char(10) NOT NULL,
	tradeDay datetime NULL,
	counterParty varchar(255) NULL,
	instrument varchar(50) NULL,
	quantity float NULL,
	quantityUOM char(20) NULL,
	strike varchar(50) NULL,
	strikeUOM char(20) NULL,
	spread float NULL,
	spreadUOM char(20) NULL,
	market varchar(255) NULL,
	lastTradingDay datetime NULL,
	deliveryDay datetime NULL,
	exerciseDay datetime NULL,
	maturityDay datetime NULL,
	callPut char(1) NULL,
	timestamp datetime NULL,
	error varchar(65536) NULL,
	bookID int NULL,
	dealID int NULL,
	strategyID int NULL,
	Period varchar(255) NULL,
	exerciseType nvarchar(15) NULL,
	timestampTo_cache datetime NULL,
	nextVersionId_cache int NULL,
	expiryDay_cache datetime NULL,
	fixedRate float NULL,
	costs varchar(8000) NULL,
	trader varchar(50) NULL,
	tradedFor varchar(50) NULL,
	broker varchar(50) NULL,
	clearinghouse varchar(50) NOT NULL,
	cleared int NULL,
	InitialPrice varchar(50) NULL,
	InitialPriceUOM varchar(20) NULL,
	PricingRule varchar(25) NULL,
	CashInstrumentType varchar(255) NULL)
  """
}
