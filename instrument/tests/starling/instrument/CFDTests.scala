package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.utils.ScalaTestUtils._
import starling.quantity.utils.QuantityTestUtils._
import starling.market._
import starling.market.Index._
import rules.CommonPricingRule
import starling.curves._
import starling.market.formula._
import starling.calendar.BrentMonth
import starling.daterange._
import starling.daterange.Day._
import starling.instrument.utils._
import starling.utils.{StarlingTest}

class CFDTests extends TestMarketTest {
  val platts_brent = Index.publishedIndexFromName("Platts Brent (April)")
  val spreadIndex = BrentCFDSpreadIndex.indexFor(new BrentMonth(4))

  val index1 = DATED_BRENT
  val index2 = platts_brent

  assertEquals(spreadIndex.indexes, Set(index1, index2))
  val market1 = index1.market
  val market2 = index2.market

  // platts forward price is higher, in contango
  val fwdPrice1 = Quantity(10, USD / BBL)
  val fwdPrice2 = Quantity(11, USD / BBL)

  val fixed1 = Quantity(3, USD / BBL)
  val fixed2 = Quantity(6, USD / BBL)

  val period = Week(2011, 2)
  val volume = Quantity(1000, BBL)
  val strike = Quantity(-2, USD / BBL)

  val swapSingle1 = new SinglePeriodSwap(
    index1,
    Quantity(0, USD / BBL),
    volume,
    period,
    true
  )
  val swapSingle2 = new SinglePeriodSwap(
    index2,
    -strike,
    volume,
    period,
    true
  )
  val swapSpread = new SinglePeriodSwap(
    spreadIndex,
    strike,
    volume,
    period,
    true, CommonPricingRule
  )

  def env(md: Day) = {
    Environment(new TestingAtomicEnvironment() {
      def marketDay = md.startOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case ForwardPriceKey(`market1`, _, _) => fwdPrice1
        case ForwardPriceKey(`market2`, _, _) => fwdPrice2
        case IndexFixingKey(`index1`, _) => fixed1
        case IndexFixingKey(`index2`, _) => fixed2
      }
    })
  }

  @Test
  def testSpreadIndexSwapInFuture {
    val envFuture = env(Day(2011, 1, 1))
    val swapSingle1Mtm = swapSingle1.mtm(envFuture)
    val swapSingle2Mtm = swapSingle2.mtm(envFuture)
    val swapSpreadMtm = swapSpread.mtm(envFuture)

    assertQtyEquals(swapSpreadMtm, (fwdPrice1 - fwdPrice2 - strike) * volume)
    assertTrue(swapSpreadMtm.isPositve)

    assertQtyEquals(swapSpreadMtm, swapSingle1Mtm - swapSingle2Mtm, 1e-6)
  }

  @Test
  def testSpreadIndexSwapMidPeriod {
    val marketDay = Day(2011, 1, 12)
    val observationDays = CommonPricingRule.observationDays(spreadIndex.calendars, period)
    val numFixed = observationDays.filter(_ < marketDay).size
    val numUnfixed = observationDays.filter(_ >= marketDay).size

    val environment = env(marketDay)
    val swapSingle1Mtm = swapSingle1.mtm(environment)
    val swapSingle2Mtm = swapSingle2.mtm(environment)
    val swapSpreadMtm = swapSpread.mtm(environment)

    val envPrice = environment.averagePrice(spreadIndex, period, CommonPricingRule, spreadIndex.priceUOM)
    val price = ((fixed1 - fixed2) * numFixed + (fwdPrice1 - fwdPrice2) * numUnfixed) / observationDays.size

    assertQtyEquals(envPrice, price, 1e-5)
    assertQtyEquals(swapSpreadMtm, (price - strike) * volume, 1e-5)
    assertQtyEquals(swapSpreadMtm, swapSingle1Mtm - swapSingle2Mtm, 1e-6)
  }

  @Test
  def testSpreadIndexPast {
    val marketDay = Day(2011, 1, 17)
    val observationDays = CommonPricingRule.observationDays(spreadIndex.calendars, period)

    val environment = env(marketDay)
    val swapSingle1Mtm = swapSingle1.mtm(environment)
    val swapSingle2Mtm = swapSingle2.mtm(environment)
    val swapSpreadMtm = swapSpread.mtm(environment)

    val envPrice = environment.averagePrice(spreadIndex, period, CommonPricingRule, spreadIndex.priceUOM)
    val price = fixed1 - fixed2

    assertQtyEquals(envPrice, price, 1e-5)
    assertQtyEquals(swapSpreadMtm, (fixed1 - fixed2 - strike) * volume)
    assertQtyEquals(swapSpreadMtm, swapSingle1Mtm - swapSingle2Mtm, 1e-6)
  }

  @Test
  def testSpreadIndexSwapSensitivitiesMidPeriod {
    val marketDay = Day(2011, 1, 12)
    val observationDays = CommonPricingRule.observationDays(spreadIndex.calendars, period)
    val numFixed = observationDays.filter(_ < marketDay).size
    val numUnfixed = observationDays.filter(_ >= marketDay).size

    val environment = env(marketDay)

    val differentiables = AtomicDatumKeyUtils.environmentDifferentiables(swapSpread, marketDay.endOfDay, USD)

    val platts_brent_april = Index.publishedIndexFromName("Platts Brent (April)")
    val diffs = Set(
      PriceDifferentiable(Index.DATED_BRENT, 13 Jan 2011), PriceDifferentiable(Index.DATED_BRENT, 14 Jan 2011),
      PriceDifferentiable(platts_brent_april, 13 Jan 2011), PriceDifferentiable(platts_brent_april, 14 Jan 2011))
    assertEquals(differentiables, diffs)
  }

//  @Test
//  def testPositionOfWeeklySwapSpanningMonths {
//    val marketDay = Day(2011, 1, 12)
//    val environment = env(marketDay)
//
//    val cfd = CFD(BrentCFDSpreadIndex.indexFor(BrentMonth(4)).get, Quantity(0, USD / BBL), Quantity(1, BBL), DateRange(Day(2011, 2, 24), Day(2011, 3, 2)))
//    val mtm = cfd.asUtpPortfolio(marketDay - 1).mtm(environment)
//    println("mtm = " + mtm)
//    cfd.asUtpPortfolio(marketDay - 1).portfolio.foreach{
//      case (utp, amt) =>
//        val (priceDiffs, volDiffs) = PivotReportUtils.priceAndVolKeys(utp, environment.marketDay, new ReportSpecificChoices())
//        priceDiffs.foreach(println)
//    }
//  }

}
