package starling.services.trade

import org.testng.Assert._
import starling.quantity.Quantity
import starling.utils.ScalaTestUtils._
import org.testng.annotations.{DataProvider, Test}
import starling.instrument._
import starling.models.{Put, Call, American}
import starling.eai.Traders
import starling.auth.User
import starling.daterange._
import starling.market.rules.{NonCommonPricingRule, CommonPricingRule}
import starling.market._

class ExcelInstrumentReaderTests extends JonTestEnv {
  @DataProvider(name = "testFutures")
  def testFuturesData = {
    val market1 = Market.NYMEX_WTI
    val market2 = Market.NYMEX_GASOLINE
    val market3 = Market.ICE_BRENT
    val market4 = FuturesSpreadMarket.ICE_WTI_BRENT
    val crack = FuturesSpreadMarket.RB_CRACKS
    constructArgs(
      (Map("id" -> 1, "size" -> -2, "unit" -> "lots", "market" -> "NYMEX WTI", "instr" -> "future", "period" -> "nov-10", "price" -> 77.84),
              Future(market1, Month(2010, 11), Quantity(77.84, market1.priceUOM), Quantity(-2000, market1.uom))),
      (Map("id" -> 2, "size" -> -2000, "unit" -> "bbl", "market" -> "NYMEX WTI", "instr" -> "future", "period" -> "nov-10", "price" -> 77.84),
              Future(market1, Month(2010, 11), Quantity(77.84, market1.priceUOM), Quantity(-2000, market1.uom))),
      (Map("id" -> 3, "size" -> -2, "unit" -> "bbl", "market" -> "NYMEX WTI", "instr" -> "future", "period" -> "x-10", "price" -> 77.84),
              Future(market1, Month(2010, 11), Quantity(77.84, market1.priceUOM), Quantity(-2, market1.uom))),
      (Map("id" -> 33, "size" -> -2, "unit" -> "bbl", "market" -> "ICE BrenT", "instr" -> "future", "period" -> "x-10", "price" -> 77.84),
              Future(market3, Month(2010, 11), Quantity(77.84, market3.priceUOM), Quantity(-2, market3.uom))),
      (Map("id" -> 333, "size" -> -2, "unit" -> "bbl", "market" -> "ICE wti vs IcE Brent", "instr" -> "future", "period" -> "x-10", "price" -> 1),
              new FuturesCommoditySpread(market4, Month(2010, 11), Quantity(1, market4.priceUOM), Quantity(-2, market4.uom))),
      (Map("id" -> 4, "size" -> -2, "unit" -> "lots", "market" -> "nymex rbob", "instr" -> "future", "period" -> "nov-10/dec-10", "price" -> "0"),
              new FuturesCalendarSpread(market2, Month(2010, 11), Month(2010, 12), Quantity(0, market2.priceUOM), Quantity(-2 * 42000, market2.uom))
              ),
      (Map("id" -> 5, "size" -> -2, "unit" -> "lots", "market" -> "nymex rbob", "instr" -> "future", "period" -> "x0/z0", "price" -> "77.21/78.31"),
              new FuturesCalendarSpread(market2, Month(2010, 11), Month(2010, 12), Quantity(77.21, market2.priceUOM), Quantity(78.31, market2.priceUOM), Quantity(-2 * 42000, market2.uom))
              ),
      (Map("id" -> 6, "size" -> -2, "unit" -> "lots", "market" -> "nymex rbob", "instr" -> "future", "period" -> "x10/z10", "price" -> "-1.02"),
              new FuturesCalendarSpread(market2, Month(2010, 11), Month(2010, 12), Quantity(0, market2.priceUOM), Quantity(1.02, market2.priceUOM), Quantity(-2 * 42000, market2.uom))
              ),
      (Map("id" -> 7, "size" -> -2, "unit" -> "lots", "market" -> "rb cracks", "instr" -> "future", "period" -> "x10", "price" -> "-1.02"),
              new FuturesCommoditySpread(crack, Month(2010, 11), Quantity(0, crack.market2.priceUOM), Quantity(1.02, crack.market2.priceUOM), Quantity(-2000, crack.market2.uom))
              )
      )
  }

  val traders = new Traders({s => Some(User.Test)})

  @Test(dataProvider = "testFutures")
  def testFutures(row: Map[String, Any], instrument: Tradeable) {
    println("ir" + instrument)
    assertEquals(ExcelTradeReader.instrument(ExcelRow(row, traders)), instrument)
  }

  @DataProvider(name = "testFuturesOptions")
  def testFuturesOptionData = {
    val market = Market.NYMEX_HEATING
    constructArgs(
      (Map("id" -> 1, "size" -> -2, "unit" -> "lots", "ex" -> "", "market" -> "nymex heat", "instr" -> "option", "period" -> "nov-10", "p/c" -> "call", "strike" -> 77.84, "price" -> 2.2),
              FuturesOption(market, market.optionExpiry(Month(2010, 11)), Month(2010, 11), Quantity(77.84, market.priceUOM), Quantity(-2 * 42000, market.uom), Call, American))
      )
  }

  @Test(dataProvider = "testFuturesOptions")
  def testFuturesOptions(row: Map[String, Any], instrument: Tradeable) {
    assertEquals(ExcelTradeReader.instrument(ExcelRow(row, traders)), instrument)
  }


  @DataProvider(name = "testSwapsData")
  def testSwapsData(): Array[Array[Object]] = {
    val index = Index.WTI10
    constructArgs(
      (Map("id" -> 1, "size" -> -2000, "unit" -> "bbl", "market" -> "NYMEX WTI 1st month", "instr" -> "swap", "period" -> "nov-10", "price" -> 77.84, "clearing house" -> "bla"),
              CommoditySwap(index, Quantity(77.84, index.priceUOM), Quantity(-2000, index.uom), Month(2010, 11), cleared = true, pricingRule = CommonPricingRule)),
      (Map("id" -> 2, "size" -> -2000, "unit" -> "bbl", "market" -> "NYMEX WTI 1st month", "instr" -> "swap", "period" -> "q410-q311", "price" -> 77.84, "clearing house" -> "", "pricing rule" -> "non-common"),
              CommoditySwap(index, Quantity(77.84, index.priceUOM), Quantity(-2000, index.uom), StripPeriod(Quarter(2010, 4).firstMonth, Quarter(2011, 3).lastMonth), cleared = false, pricingRule = NonCommonPricingRule)),
      (Map("id" -> 3, "size" -> -2000, "unit" -> "bbl", "market" -> "NYMEX WTI 1st month", "instr" -> "swap", "period" -> "cal-11", "price" -> 77.84, "clearing house" -> ""),
              CommoditySwap(index, Quantity(77.84, index.priceUOM), Quantity(-2000, index.uom), StripPeriod(Year(2011).firstMonth, Year(2011).lastMonth), cleared = false, pricingRule = CommonPricingRule))
      )
  }

  @Test(dataProvider = "testSwapsData")
  def testSwaps(row: Map[String, Any], instrument: Tradeable) {
    assertEquals(ExcelTradeReader.instrument(ExcelRow(row, traders)), instrument)
  }


  @DataProvider(name = "testAsianData")
  def testAsianData(): Array[Array[Object]] = {
    val index = Index.WTI10
    constructArgs(
      (Map("id" -> 1, "size" -> -2000, "unit" -> "bbl", "ex" -> "", "market" -> "NYMEX WTI 1st month", "instr" -> "asian", "period" -> "nov-10", "strike" -> 77.84, "p/c" -> "put"),
              AsianOption(index, Month(2010, 11), Quantity(77.84, index.priceUOM), Quantity(-2000, index.uom), Put)),
      (Map("id" -> 2, "size" -> -2000, "unit" -> "bbl", "ex" -> "", "market" -> "NYMEX WTI 1st month", "instr" -> "asian", "period" -> "q410-q311", "strike" -> 77.84, "p/c" -> "put"),
              AsianOption(index, StripPeriod(Quarter(2010, 4), Quarter(2011, 3)), Quantity(77.84, index.priceUOM), Quantity(-2000, index.uom), Put)),
      (Map("id" -> 3, "size" -> -2000, "unit" -> "bbl", "ex" -> "", "market" -> "NYMEX WTI 1st month", "instr" -> "asian", "period" -> "cal-11", "strike" -> 77.84, "p/c" -> "put"),
              AsianOption(index, StripPeriod(Month(2011, 1), Month(2011, 12)), Quantity(77.84, index.priceUOM), Quantity(-2000, index.uom), Put))
      )
  }

  @Test(dataProvider = "testAsianData")
  def testAsians(row: Map[String, Any], instrument: Tradeable) {
    println("???")
    val excelRow = ExcelRow(row, traders)
    assertEquals(ExcelTradeReader.instrument(excelRow), instrument)
  }

  @Test(expectedExceptions = Array(classOf[AssertionError]))
  def testNegativePrices1 {
    val future = Map("id" -> 6, "size" -> -2, "unit" -> "lots", "market" -> "nymex rbob", "instr" -> "future", "period" -> "x10", "price" -> "-1.02")
    ExcelRow(future, traders).price
  }

  @Test(expectedExceptions = Array(classOf[AssertionError]))
  def testNegativePrices2 {
    val future = Map("id" -> 6, "size" -> -2, "unit" -> "lots", "market" -> "nymex rbob", "instr" -> "futures option", "period" -> "x10", "price" -> "-1.02")
    ExcelRow(future, traders).price
  }

  @Test(expectedExceptions = Array(classOf[AssertionError]))
  def testNegativePrices3 {
    val future = Map("id" -> 6, "size" -> -2, "unit" -> "lots", "market" -> "NYMEX WTI 1st month", "instr" -> "swap", "period" -> "x10", "price" -> "-1.02")
    ExcelRow(future, traders).price
  }

  @Test
  def testNegativePrices4 {
    val future = Map("id" -> 6, "size" -> -2, "unit" -> "lots", "market" -> "nymex rbob", "instr" -> "future", "period" -> "x10/z10", "price" -> "-1.02")
    assertEquals (ExcelRow(future, traders).price, Quantity(-1.02, Market.NYMEX_GASOLINE.priceUOM))
  }

  @Test
  def testNegativePrices5 {
    val future = Map("id" -> 6, "size" -> -2, "unit" -> "bbls", "market" -> "Gas Oil Crack", "instr" -> "swap", "period" -> "x10/z10", "price" -> "-1.02")
    assertEquals (ExcelRow(future, traders).price, Quantity(-1.02, Index.IPE_GAS_OIL_VS_IPE_BRENT.priceUOM))
  }
}