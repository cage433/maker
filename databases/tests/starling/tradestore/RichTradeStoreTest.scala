package starling.tradestore

import eai.EAITradeAttributes._
import eai.{EAITradeAttributes, EAITradeStore}
import org.scalatest.FunSuite
import org.scalatest.testng.TestNGSuite
import java.sql.Connection
import org.springframework.jdbc.datasource.SingleConnectionDataSource
import starling.richdb.{RichResultSetRowFactory, RichDB}
import starling.manager.Broadcaster
import swing.event.Event
import org.mockito.Mockito._
import org.mockito.Matchers._
import starling.eai.{TreeID, EAIStrategyDB}
import starling.pivot.PivotTreePath._
import starling.eai.TreeID._
import starling.daterange.{Timestamp, Day}
import starling.tradeimport.ClosedDesks
import starling.gui.api.Desk
import starling.instrument.Trade._
import starling.instrument.TradeID._
import starling.instrument.ErrorInstrument._
import starling.db.{TradeStoreTest, EAITradeSystem, TestDB, DBTest}
import org.testng.annotations.Test
import starling.pivot.PivotTreePath
import starling.instrument._
import starling.market.TestMarketTest
import starling.quantity.Quantity._
import starling.quantity.{UOM, Quantity}
import org.testng.Assert._


class RichTradeStoreTest extends TestMarketTest with TestNGSuite {

  @Test
  def testPivot {
    val connection: Connection = DBTest.getConnection("jdbc:h2:mem:RichTradeStoreTest;create=true")
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
    val close1 = Day(2011, 1, 1).toTimestamp
    val close2 = Day(2011, 1, 2).toTimestamp
    val close3 = Day(2011, 1, 3).toTimestamp
    val close4 = Day(2011, 1, 4).toTimestamp
    val closes = List(close1, close2, close3, close4)

    val cd = mock(classOf[ClosedDesks])
    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1).map(c => (day, c, None))

    val store = new EAITradeStore(db, broadcaster, eAIStrategyDB, Desk.GasolineSpec, cd)

    val attr = EAITradeAttributes(TreeID(1), TreeID(149), TreeID(3), "trader", "tradedfor", "broker", "clearinghouse")

    val trade1a = Trade(TradeID(1, EAITradeSystem), Day(2011, 1, 1), "cp", attr, ErrorInstrument("error1"))
    val trade2a = Trade(TradeID(2, EAITradeSystem), Day(2011, 1, 1), "cp", attr, CommoditySwap.sample)
    val trade3a = Trade(TradeID(3, EAITradeSystem), Day(2011, 1, 1), "cp", attr, Future.sample)

    val tradesA = List(trade1a, trade2a, trade3a)
    val storedA = store.storeTrades((trade) => true, tradesA, close1)

    // change a trade
    val trade1b = trade1a.copy(attributes = attr.copy(strategyID = TreeID(66)))
    val trade2b = trade2a.copy()
    val trade3b = trade3a.copy(tradeable = ErrorInstrument("error3"))

    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1, close2).map(c => (day, c, None))
    val tradesB = List(trade1b, trade2b, trade3b)
    val storedB = store.storeTrades((trade) => true, tradesB, close2)

    // delete and change a trade
    val trade2c = trade2b.copy(tradeable = CommoditySwap.sample.copy(_volume = Quantity(12.123, UOM.MT)))
    val trade3c = trade3b.copy()

    val tradesC = List(trade2c, trade3c)
    when(cd.closesForDesk(any(classOf[Desk]))) thenReturn List(close1, close2, close3).map(c => (day, c, None))
    val storedC = store.storeTrades((trade) => true, tradesC, close3)

    val changes2 = store.tradeChanges(close1, close3, Day(2011, 1, 1), TradePredicate.Null)
    assertEquals(changes2.deleted.size, 1)
    assertEquals(changes2.amended.size, 2)
    assertEquals(changes2.changes, 3)
    val changes1 = store.tradeChanges(close1, close2, Day(2011, 1, 1), TradePredicate.Null)
    assertEquals(changes1.amended.size, 2)
    assertEquals(changes1.changes, 2)
    val changes3 = store.tradeChanges(close2, close3, Day(2011, 1, 1), TradePredicate.Null)
    assertEquals(changes3.deleted.size, 1)
    assertEquals(changes3.amended.size, 1)
    assertEquals(changes3.changes, 2)
    val changes4 = store.tradeChanges(new Timestamp(1234), close1, Day(2011, 1, 1), TradePredicate.Null)
    assertEquals(changes4.created.size, 3)
    assertEquals(changes4.changes, 3)

    db.inTransaction {
      writer => {
        writer.update("drop table EAITRADE_BOOK_149")
      }
    }
    connection.close()
  }
}