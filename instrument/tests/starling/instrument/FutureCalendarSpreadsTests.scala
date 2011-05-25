package starling.instrument

import starling.curves._
import starling.daterange.{Day, Month}
import starling.daterange.Day._
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}
import starling.market._
import starling.varcalculator.ForwardPriceRiskFactor

class FutureCalendarSpreadsTests extends JonTestEnv {
  import org.testng.annotations._
  import org.testng.Assert._

  val env = makeEnv((1 Jan 2010).endOfDay)
  val market = Market.NYMEX_WTI
  val period1 = Month(2010, 3)
  val period2 = Month(2010, 12)
  val price1 = env.forwardPrice(market, period1)
  val price2 = env.forwardPrice(market, period2)

  @Test
  def testFuturesMtm1 {
    val spread = price1 - price2
    assertEquals(spread, env.spreadPrice(market, period1, period2))

    val f1 = Future(market, period1, spread.copy(value = 0.0), 1(BBL))
    val f2 = Future(market, period2, -spread, (-1)(BBL))
    val fs = new FuturesCalendarSpread(market, period1, period2, spread, 1(BBL))

    val fmtm = f1.mtm(env) + f2.mtm(env)
    val mtm = fs.mtm(env)
    val spreadValue = (price1 - price2 - spread) * 1(BBL)
    assertQtyEquals(mtm, spreadValue, 1e-6)
  }

  @Test
  def testFuturesMtmProfit {
    val spread = price1 - price2

    val f1 = Future(market, period1, spread.copy(value = 0.0), 1(BBL))
    val f2 = Future(market, period2, -spread, (-1)(BBL))

    val f3 = Future(market, period1, price1, 1(BBL))
    val f4 = Future(market, period2, price2, (-1)(BBL))

    val fs = new FuturesCalendarSpread(market, period1, period2, spread, 1(BBL))
    val env = this.env.shiftPrice(market, period2, (-1)(USD / BBL))

    val fmtm = f1.mtm(env) + f2.mtm(env)
    val fmtm2 = f3.mtm(env) + f4.mtm(env)
    val mtm = fs.mtm(env)
    val spreadValue = (price1 - (price2 - (1)(USD / BBL)) - spread) * 1(BBL)
    assertQtyEquals(mtm, fmtm, 1e-6)
    assertQtyEquals(mtm, fmtm2, 1e-6)
    assertQtyEquals(mtm, spreadValue, 1e-6)
    assertQtyEquals(mtm, 1(USD), 1e-6)
  }

  @Test
  def testFuturesMtmLoss {
    val spread = price1 - price2

    val f1 = Future(market, period1, spread.copy(value = 0.0), 1(BBL))
    val f2 = Future(market, period2, -spread, (-1)(BBL))

    val f3 = Future(market, period1, price1, 1(BBL))
    val f4 = Future(market, period2, price2, (-1)(BBL))

    val fs = new FuturesCalendarSpread(market, period1, period2, spread, 1(BBL))
    val env = this.env.shiftPrice(market, period1, (-1)(USD / BBL))

    val fmtm = f1.mtm(env) + f2.mtm(env)
    val fmtm2 = f3.mtm(env) + f4.mtm(env)

    val mtm = fs.mtm(env)
    val spreadValue = ((price1 - (1)(USD / BBL)) - price2 - spread) * 1(BBL)
    assertQtyEquals(mtm, fmtm, 1e-6)
    assertQtyEquals(mtm, fmtm2, 1e-6)
    
    assertQtyEquals(mtm, spreadValue, 1e-6)
    assertQtyEquals(mtm, (-1)(USD), 1e-6)
  }


}