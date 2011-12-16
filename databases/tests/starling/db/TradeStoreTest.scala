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
import starling.manager.Broadcaster
import starling.daterange._
import starling.instrument.{TradeID, Trade, TradeSystem}
import starling.tradestore.eai.{EAITradeStore, EAITradeAttributes}
import starling.pivot.{PivotTreePath, SomeSelection, Field}
import starling.eai.{EAIStrategyDB, TreeID}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.testng.annotations.{AfterMethod, BeforeMethod, Test}
import java.sql.Connection
import starling.props.PropsHelper
import starling.gui.api.{Desk, EAIDeskInfo}
import starling.marketdata.{MarketDataTypes, ReferenceDataLookup}
import org.scalatest.testng.TestNGSuite
import starling.tradeimport.ClosedDesks

class TradeStoreTest extends TestMarketTest with TestNGSuite {

  @Test
  def testStoreTrades {
    val connection: Connection = DBTest.getConnection("jdbc:h2:mem:EAITradeStoreTest;create=true")
    val ds = new SingleConnectionDataSource(connection, true)
    var db: RichDB = new TestDB(ds, new RichResultSetRowFactory)
    db.inTransaction {
      writer => {
        writer.update(TradeStoreTest.create_table)
      }
    }

    val broadcaster = new Broadcaster() {
      def broadcast(event: Event) = {}
    }

    val eAIStrategyDB = mock(classOf[EAIStrategyDB])
    when(eAIStrategyDB.pathFor(any(classOf[TreeID]))) thenReturn PivotTreePath("test [54418]")
    when(eAIStrategyDB.getStrategyFromDealId(any(classOf[TreeID]))) thenReturn Some(TreeID(54418))

    val day = Day.today
    val close1 = new Timestamp(1)
    val close2 = new Timestamp(2)
    val close3 = new Timestamp(3)
    val close4 = new Timestamp(4)
    val closes = List(close1, close2, close3, close4)

    val cd = mock(classOf[ClosedDesks])
    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1).map(c => (day, c, None))

    val store = new EAITradeStore(db, broadcaster, eAIStrategyDB, Desk.GasolineSpec, cd)

    val attr = EAITradeAttributes(TreeID(1), TreeID(149), TreeID(3), "trader", "tradedfor", "broker", "clearinghouse")

    val trade1a = Trade(TradeID(1, EAITradeSystem), Day.today, "cp", attr, ErrorInstrument("error1"))
    val trade2a = Trade(TradeID(2, EAITradeSystem), Day.today, "cp", attr, CommoditySwap.sample)
    val trade3a = Trade(TradeID(3, EAITradeSystem), Day.today, "cp", attr, Future.sample)

    val tradesA = List(trade1a, trade2a, trade3a)
    val storedA = store.storeTrades((trade) => true, tradesA, close1)

    assertEquals(storedA.inserted, 3)
    assertEquals(storedA.updated, 0)
    assertEquals(storedA.deleted, 0)
    assertEquals(storedA.changed, true)

    val tradesResultA = store.readLatestVersionOfAllTrades()
    assertEquals(tradesResultA.values.map(_.trade).toSet, tradesA.toSet)

    // change a trade
    val trade1b = trade1a.copy(attributes = attr.copy(strategyID = TreeID(66)))
    val trade2b = trade2a.copy()
    val trade3b = trade3a.copy(tradeable = ErrorInstrument("error3"))

    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1, close2).map(c => (day, c, None))
    val tradesB = List(trade1b, trade2b, trade3b)
    val storedB = store.storeTrades((trade) => true, tradesB, close2)

    assertEquals(storedB.inserted, 0)
    assertEquals(storedB.updated, 2)
    assertEquals(storedB.deleted, 0)
    assertEquals(storedB.changed, true)

    val tradesResultB = store.readLatestVersionOfAllTrades()
    assertEquals(tradesResultB.values.map(_.trade).toSet, tradesB.toSet)

    // delete and change a trade
    val trade2c = trade2b.copy(tradeable = CommoditySwap.sample.copy(_volume = Quantity(12.123, MT)))
    val trade3c = trade3b.copy()

    val tradesC = List(trade2c, trade3c)
    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1, close2, close3).map(c => (day, c, None))
    val storedC = store.storeTrades((trade) => true, tradesC, close3)

    assertEquals(storedC.inserted, 0)
    assertEquals(storedC.updated, 1)
    assertEquals(storedC.deleted, 1)
    assertEquals(storedC.changed, true)

    val tradesResultC = store.readLatestVersionOfAllTrades()


    assertEquals(tradesResultC.values.map(_.trade).toSet, tradesC.toSet)

    // do nothing
    val tradesD = tradesC
    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1, close2, close3, close4).map(c => (day, c, None))
    val storedD = store.storeTrades((trade) => true, tradesD, close4)

    assertEquals(storedD.inserted, 0)
    assertEquals(storedD.updated, 0)
    assertEquals(storedD.deleted, 0)
    assertEquals(storedD.changed, false)

    val tradesResultD = store.readLatestVersionOfAllTrades()
    assertEquals(tradesResultD.values.map(_.trade).toSet, tradesD.toSet)

    val history = store.singTradeHistory(TradeID(1, EAITradeSystem))
    assertEquals(history(close1).trade, trade1a)
    assertEquals(history(close2).trade, trade1b)
    assertEquals(history(close3).trade.tradeable.tradeableType, DeletedInstrument)

    assertEquals(store.readTrade(TradeID(1, EAITradeSystem), Some(close1)).get.trade, trade1a)
    assertEquals(store.readTrade(TradeID(1, EAITradeSystem), Some(close2)).get.trade, trade1b)
    assertEquals(store.readTrade(TradeID(1, EAITradeSystem), None).get.trade.tradeable.tradeableType, DeletedInstrument)

    db.inTransaction {
      writer => {
        writer.update("drop table EAITRADE_BOOK_149")
      }
    }
    connection.close()
  }
}

object TradeStoreTest {

  val create_table = """
    CREATE TABLE EAITRADE_BOOK_149
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
	RoundingMethodRule varchar(25) NULL,
	roundingOverride tinyint NULL,
	CashInstrumentType varchar(255) NULL)
  """
}
