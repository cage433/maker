package starling.services.trade

import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.RichQuantity._
import starling.quantity.UOM._
import starling.utils.ScalaTestUtils._
import org.testng.annotations.{DataProvider, Test}
import starling.market.Market._
import starling.daterange.Day._
import starling.instrument._
import starling.daterange._
import starling.eai.TreeID
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.tradestore.TradeAndFields
import starling.utils.{AppendingMap, StarlingTest}
import starling.instrument.{TradeID, TradeSystem, Trade, TradeAttributes}

class TradeDiffTests extends StarlingTest {
  private def tradeIDs(ts: (TradeAndFields, TradeAndFields)) = (ts._1.trade.tradeID, ts._2.trade.tradeID)

  val attrA = IntradayTradeAttributes(Some(TreeID(123)), TreeID(321), Some(TreeID(1234)), "jon fox", "jon fox", "broker", "some comment", "clearer", "some group", Day.today, "user")

  def toTradeAndFields(trade:Trade) = TradeAndFields(1, trade)

  @Test
  def testMultiple {
    val ts = new TradeSystem("asdf", "asdf") {}
    val a1 = Trade(TradeID(1, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val a2 = Trade(TradeID(2, ts), 02 Oct 2010, "ttf1", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val a3 = Trade(TradeID(3, ts), 01 Oct 2010, "ttf1", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val b1 = Trade(TradeID(10, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val b2 = Trade(TradeID(11, ts), 02 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())

    val report = new TradeDiff(List(), List(a1, a2, a3).map(toTradeAndFields), List(b2, b1).map(toTradeAndFields))

    assertEquals(report.matches.map(tradeIDs), List((a1.tradeID, b1.tradeID)))
    assertEquals(report.nearMatches.map(tradeIDs), List((a2.tradeID, b2.tradeID)))
    assertEquals(report.unmatched.map(_.trade.tradeID), List(a3.tradeID))
  }

  @Test
  def testNoDiffs {
    val ts = new TradeSystem("asdf", "asdf") {}

    val a1 = Trade(TradeID(1, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val b1 = Trade(TradeID(10, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())

    val report = new TradeDiff(List(), List(a1).map(toTradeAndFields), List(b1).map(toTradeAndFields))

    assertEquals(report.matches.map(tradeIDs), List((a1.tradeID, b1.tradeID)))
    assertEquals(report.nearMatches, Nil)
    assertEquals(report.unmatched, Nil)
  }

  @Test
  def test1Diffs {
    val ts = new TradeSystem("asdf", "asdf") {}
    val a1 = Trade(TradeID(1, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val b1 = Trade(TradeID(10, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13001.00)(BBL)), List())

    val report = new TradeDiff(List(), List(a1).map(toTradeAndFields), List(b1).map(toTradeAndFields))

    assertEquals(report.matches, Nil)
    assertEquals(report.nearMatches.map(tradeIDs), List((a1.tradeID, b1.tradeID)))
    assertEquals(report.unmatched, Nil)
  }

  @Test
  def test2Diffs {
    val ts = new TradeSystem("asdf", "asdf") {}

    val a1 = Trade(TradeID(1, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13000.00)(BBL)), List())
    val b1 = Trade(TradeID(10, ts), 02 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 77.13(USD / BBL), -(13001.00)(BBL)), List())

    val report = new TradeDiff(List(), List(a1).map(toTradeAndFields), List(b1).map(toTradeAndFields))

    assertEquals(report.matches, Nil)
    assertEquals(report.nearMatches.map(tradeIDs), List((a1.tradeID, b1.tradeID)))
    assertEquals(report.unmatched, Nil)
  }

  @Test
  def testMultiLeg {
    val ts = new TradeSystem("asdf", "asdf") {}

    val futuresSpread = FuturesCalendarSpread(NYMEX_WTI, Month(2010, 11), Month(2010, 12), 25(USD / BBL), 28(USD / BBL), 13000.00(BBL))
    val a1 = Trade(TradeID(1, ts), 01 Oct 2010, "ttf", attrA, futuresSpread, List())
    val b1 = Trade(TradeID(10, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 11), 25(USD / BBL), 13000.00(BBL)), List())
    val b2 = Trade(TradeID(10, ts), 01 Oct 2010, "ttf", attrA, Future(NYMEX_WTI, Month(2010, 12), 28(USD / BBL), -(13000.00)(BBL)), List())

    val report = new TradeDiff(List(), List(a1).map(toTradeAndFields), List(b2, b1).map(toTradeAndFields))

    val a1Leg1 = a1.copy(tradeable = futuresSpread.frontFuture)
    val a1Leg2 = a1.copy(tradeable = futuresSpread.backFuture)
    assertEquals(report.matches.map(t => (t._1.trade, t._2.trade)).toSet, Set((a1Leg1, b1), (a1Leg2, b2)))
    assertEquals(report.nearMatches, Nil)
    assertEquals(report.unmatched, Nil)
  }
}