package starling.instrument

import starling.curves._
import starling.daterange.Day._
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}
import starling.quantity.UOM._
import starling.market._
import starling.models.Call
import starling.market.Market._
import org.testng.annotations.Test
import org.testng.Assert._
import starling.daterange.{SpreadPeriod, StripPeriod, Day, Month, Quarter}

class MultiLegTests extends JonTestEnv {
  val env = makeEnv(Day(2010, 1, 1).endOfDay)

  @Test
  def testCSO {
    val jan = Month(2011, 1)
    val feb = Month(2011, 2)
    val mar = Month(2011, 3)
    val period = new StripPeriod(SpreadPeriod(jan, feb), SpreadPeriod(feb, mar))
    val cso = new CalendarSpreadOption(NYMEX_WTI, period, (-1)(USD / BBL), 1(BBL), Call)
    assertTrue(cso.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) > Quantity(1, USD), cso.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) + " !> 1")
    assertQtyEquals(cso.legs.map(_.asUtpPortfolio(Day(2009, 1, 1)).mtm(env)).sum, cso.asUtpPortfolio(Day(2009, 1, 1)).mtm(env))
  }

  @Test
  def testFutureSpread {
    val jan = Month(2011, 1)
    val feb = Month(2011, 2)
    val fs = new FuturesCalendarSpread(NYMEX_WTI, jan, feb, (-2)(USD / BBL), 1(BBL))
    assertTrue(fs.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) > Quantity(1, USD), fs.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) + " !> 1")
    assertQtyEquals(fs.legs.map(_.asUtpPortfolio.mtm(env)).sum, fs.asUtpPortfolio(Day(2009, 1, 1)).mtm(env))
  }

  @Test
  def testSwap {
    val index = Index.WTI10
    val strike = 10(USD / BBL)
    val volume = 1(BBL)
    val period = Quarter(2011, 1)
    val cs = new CommoditySwap(index, strike, volume, StripPeriod(period.firstMonth, period.lastMonth), cleared = false)
    val scs = Quarter(2011, 1).toListOfMonths.map(m => SinglePeriodSwap(index, strike, volume, m, cleared = false))
    assertTrue(cs.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) > Quantity(1, USD))
    assertQtyEquals(cs.legs.map(_.asUtpPortfolio(Day(2009, 1, 1)).mtm(env)).sum, cs.asUtpPortfolio(Day(2009, 1, 1)).mtm(env), 1e-10)
    assertQtyEquals(scs.map(_.mtm(env)).sum, cs.asUtpPortfolio(Day(2009, 1, 1)).mtm(env), 1e-10)
  }

  @Test
  def testAsian {
    val index = Index.WTI10
    val jan = Month(2011, 1)
    val feb = Month(2011, 2)
    val mar = Month(2011, 3)
    val period = new StripPeriod(jan, mar)

    val fp = env.forwardPrice(index.market, jan)
    val volume = 1(BBL)

    val ao = new AsianOption(index, period, fp, volume, Call)
    val sao = Quarter(2011, 1).toListOfMonths.map(m => SingleAsianOption(index, m, fp, volume, Call))
    assertTrue(ao.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) > Quantity(1, USD))
    assertQtyEquals(ao.legs.map(_.asUtpPortfolio(Day(2009, 1, 1)).mtm(env)).sum, ao.asUtpPortfolio(Day(2009, 1, 1)).mtm(env))
    assertQtyEquals(sao.map(_.mtm(env)).sum, ao.asUtpPortfolio(Day(2009, 1, 1)).mtm(env))
  }
}