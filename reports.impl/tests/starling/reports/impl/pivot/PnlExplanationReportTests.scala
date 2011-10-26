package starling.reports.impl.pivot

import org.testng.Assert._
import starling.daterange.{Month, DayAndTime, Day}
import starling.instrument._
import starling.curves._
import starling.quantity.{Percentage, Quantity, UOM}
import starling.quantity.UOM._
import starling.quantity.utils.QuantityTestUtils._
import starling.concurrent.MP
import org.testng.annotations.{AfterTest, AfterClass, Test}
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.quantity.Quantity._
import starling.daterange.Day._
import starling.reports.impl.pivot.PivotReport._
import starling.market._
import starling.models.{American, European, Call}
import starling.utils.{ StarlingTest}

class PnlExplanationReportTests extends JonTestEnv {
  def noChoices = ReportSpecificChoices()

  @Test
  def testZeroPnlComponentsAreRemoved() {
    //don't change the interest rate so there is 0 pnl due to interest rates
    val market = Market.LME_LEAD
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.0)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 30, 0.0)

    val forward = new Future(market, Day(2010, 1, 1), Quantity(0, market.priceUOM), Quantity(1, market.uom))

    val plainPnl = forward.mtm(Environment(env2)) - forward.mtm(Environment(env1))
    val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> forward))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), forward), noChoices)
    val explanationSum = Quantity.sum(pnlExplanation.map(_.pnl.quantityValue.get))
    assertEquals(explanationSum, plainPnl)
    pnlExplanation.map { row => assertFalse(row.pnl.isAlmostZero) }
  }

  @Test
  def testMarketChangesForFutureCrossTerms() {
    //make a huge interest rate change to get a price vs interest rate cross term
    val market = Market.LME_LEAD
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.01)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 30, 0.10)

    val forward = new Future(market, Day(2010, 1, 1), Quantity(0, market.priceUOM), Quantity(1, market.uom))

    val plainPnl = forward.mtm(Environment(env2)) - forward.mtm(Environment(env1))
    val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> forward))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), forward), noChoices)
    val explanationSum = Quantity.sum(pnlExplanation.map(_.pnl.quantityValue.get))
    assertEquals(explanationSum, plainPnl)
  }

  class InstrumentWithIncorrectAtomicKeys(day:Day) extends UTP {
    def pivotUTPType = throw new Exception

    protected def explanation(env: Environment) = throw new Exception

    def instrumentType = throw new Exception
    def asUtpPortfolio = throw new Exception
    def isLive(dayAndTime: DayAndTime) = true
    def valuationCCY = UOM.USD

    override def atomicMarketDataKeys(env : Environment, ccy : UOM): Set[AtomicDatumKey] = Set()
    def assets(env: Environment) = {
      val market = Market.LME_LEAD
      Assets(Asset.estimatedCash(day, env.forwardPrice(market, day) * Quantity(1, market.uom), env))
    }

    def deltaStepType() = throw new Exception

    def price(env: Environment) = throw new Exception

    def volume = new Quantity(1.0)

    def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = throw new Exception

    def * (scale : Double) = this
    def periodKey = None
  }

  @Test
  def testUnexplainedRowUsedWhenInstrumentsKeysAreWrong() {
    //Use the InstrumentWithIncorrectAtomicKeys which does not report its atomicMarketDataKeys so the pnl is unexplained
    val market = Market.LME_LEAD
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.00)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 30, 0.00)

    val instrument = new InstrumentWithIncorrectAtomicKeys(Day(2010, 1, 1))

    val plainPnl = instrument.mtm(Environment(env2)) - instrument.mtm(Environment(env1))
    val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> instrument))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), instrument), noChoices)
    val explanationSum = Quantity.sum(pnlExplanation.map(_.pnl.quantityValue.get))
    assertEquals(explanationSum, plainPnl)
    assertTrue(pnlExplanation.map{_.componentName}.contains("Other changes"))
  }

  @Test
  def testDeltaPnlOnForward() {
    val market = Market.LME_LEAD
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.0)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 30, 0.0)

    val volume = Quantity(1, market.uom)
    val priceChange = Quantity(30 - 20, market.priceUOM)
    val forward = new Future(market, Day(2010, 1, 1), Quantity(0, market.priceUOM), volume)

    val deltaPnl = volume * priceChange

    val plainPnl = forward.mtm(Environment(env2)) - forward.mtm(Environment(env1))
      val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> forward))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), forward), noChoices)
    assertEquals(pnlExplanation.find{e => e.componentName == "Delta DC"}.get.pnl.quantityValue.get, deltaPnl)
    assertFalse(pnlExplanation.find{r=>r.componentName=="Delta DC" && r.period == None}.isDefined, "Expected a higher order term: " + pnlExplanation.map{_.componentName})
  }

  @Test
  def testRhoPnlOnForward() {
    val market = Market.ICE_WTI
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.1)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.2)

    val volume = Quantity(1, market.uom)
    val forward = new SinglePeriodSwap(Index.WTI10, Quantity(0, market.priceUOM), volume, Month(2010, 1), cleared = false)

    val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> forward))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), forward), noChoices)
    assertTrue(pnlExplanation.find{_.componentName == "Rho DC"}.isDefined, "Expect pnl due to interest rate change")
  }

  @Test
  def testDeltaPnlOnForwardOption() {
    val market = Market.LME_LEAD
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2008, 9, 15).endOfDay, market, 20, 0.0, 0.2)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2008, 9, 15).endOfDay, market, 30, 0.0, 0.2)

    val option = new FuturesOption(market, Day(2009, 10, 1), Day(2009, 10, 5), Quantity(10, market.priceUOM), Quantity(1000, market.uom), Call, European)

    val plainPnl = option.mtm(Environment(env2)) - option.mtm(Environment(env1))
    val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> option))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), option), noChoices)
    //There was a large price change so there should be cross terms
    val crossPnl = pnlExplanation.find{r=>r.componentName=="Cross Terms"}
    assertTrue(crossPnl.isDefined)
  }

   @Test
  def testGammaPnlOnForwardOption() {
    val market = Market.LME_LEAD
    val env1 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2008, 9, 15).endOfDay, market, 20, 0.0, 0.2)
    val env2 = TestEnvironmentBuilder.curveObjectEnvironment(Day(2008, 9, 15).endOfDay, market, 21, 0.0, 0.2)

    val option = new FuturesOption(market, Day(2009, 10, 1), Day(2009, 10, 5), Quantity(10, market.priceUOM), Quantity(1000, market.uom), Call, European)

    val plainPnl = option.mtm(Environment(env2)) - option.mtm(Environment(env1))
    val marketChangesPnl = new MarketChangesPnl(env1, env2, Map(UTPIdentifier(1) -> option))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), option), noChoices)
    assertTrue(pnlExplanation.find{r=>r.componentName=="Delta DC" && r.period != None}.isDefined, "Expect a delta pnl")
    assertTrue(pnlExplanation.find{r=>r.componentName=="Gamma DC" && r.period != None}.isDefined, "Expect a gamma pnl")
  }

  @Test
  def testTimePnlOnForward() {
    val market = Market.LME_LEAD
    val env = Environment(TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.20))
    val forwardDay = env.marketDay.day.nextDay

    val forward = new Future(market, Day(2012, 1, 1), Quantity(0, market.priceUOM), Quantity(1000, market.uom))

    val expectedInterest = forward.mtm(env) * (1-env.discount(market.currency, forwardDay))

    val timeChangesPnl = new TimeChangesPnl(env, env.marketDay.nextDay, Map(UTPIdentifier(1) -> forward))
    val pnlExplanation = timeChangesPnl.combine(timeChangesPnl.rows(UTPIdentifier(1), forward), noChoices)
    pnlExplanation.foreach { row =>
      row.label match {
        case "Theta DC" => assertQtyEquals(row.scaledPnl.quantityValue.get, expectedInterest, 0.1)
      }
    }
  }

  def testTimePnlOnForwardOverDeliveryDay() {
    val market = Market.LME_LEAD
    val env = Environment(TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.20))
    val forwardDay = env.marketDay.day.nextDay

    val forward = new Future(market, env.marketDay.day, Quantity(0, market.priceUOM), Quantity(1000, market.uom))

    val expectedInterest = forward.mtm(env) * (1-env.discount(market.currency, forwardDay))

      val timeChangesPnl = new TimeChangesPnl(env, env.marketDay.nextDay, Map(UTPIdentifier(1) -> forward))
    val pnlExplanation = timeChangesPnl.rows(UTPIdentifier(1), forward)
    pnlExplanation.foreach { row =>
      row match {
        case TimeChangesPnlRow(_, _, "Theta DC", pnl, _, _, _, _) => assertQtyEquals(pnl.quantityValue.get, expectedInterest, 0.1)
        case TimeChangesPnlRow(_, _, "Expiry DC", pnl, _, _, _, _) => assertQtyEquals(pnl.quantityValue.get, Quantity(0, UOM.USD), 1e-6)
      }
    }
  }

  @Test
  def testTimePnlOnFuture() {
    val market = Market.NYMEX_WTI
    val env = TestEnvironmentBuilder.curveObjectEnvironment(Day(2009, 9, 15).endOfDay, market, 20, 0.20)
    val forwardDay = env.marketDay.day.nextDay

    val future = new Future(market, Month(2010, 12), Quantity(10, market.priceUOM), Quantity(1000, UOM.BBL))

      val timeChangesPnl = new TimeChangesPnl(Environment(env), env.marketDay.nextDay, Map(UTPIdentifier(1) -> future))
    val pnlExplanation = future.asUtpPortfolio.portfolio.keys.flatMap{utp => timeChangesPnl.combine(timeChangesPnl.rows(UTPIdentifier(1), utp), noChoices)}.toList
    assertEquals(pnlExplanation, List())
  }

  @Test
  def testTimePnlOnForwardOption() {
    val market = Market.LME_LEAD
    val env = Environment(new TestingAtomicEnvironment(){
      def marketDay = Day(2009, 9, 1).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case _ : ForwardPriceKey => Quantity(10, market.priceUOM)
      }
    }).undiscounted
    val forwardDay = env.marketDay.day + 20

    val option = new FuturesOption(market, Day(2009, 10, 1), Day(2009, 10, 5), Quantity(10, market.priceUOM), Quantity(1000, market.uom), Call, European)


    val timeChangesPnl = new TimeChangesPnl(env, env.marketDay.nextDay, Map(UTPIdentifier(1) -> option))
    val pnlExplanation = timeChangesPnl.combine(timeChangesPnl.rows(UTPIdentifier(1), option), noChoices)
    pnlExplanation.foreach { row =>
      row.label match {
        case "Theta DC" => assertTrue(row.scaledPnl.quantityValue.get < Quantity(-0.1, UOM.USD), "Option theta should be negative")
        case "Expiry DC" => assertEquals(row.scaledPnl, Quantity(0, UOM.USD))
      }
    }
  }

  @Test
  def testTimePnlOnForwardOptionOverOptionExpiry() {
    val market = Market.LME_LEAD
    val env = Environment(new TestingAtomicEnvironment(){
      def marketDay = Day(2009, 9, 1).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case _ : ForwardPriceKey => Quantity(10, market.priceUOM)
      }
    }).undiscounted.zeroVols

    //An in the money call option expiring during the pnl period
    //Payoff is 10-5 * 1000 which will be lost after expiry
    //In practise a new forward should be created but for this trade there is an expiry pnl of -5000
    val option = new FuturesOption(market, Day(2009, 9, 10), Day(2009, 10, 5), Quantity(5, market.priceUOM), Quantity(1000, market.uom), Call, European)

  val timeChangesPnl = new TimeChangesPnl(env, env.marketDay + 20, Map(UTPIdentifier(1) -> option))
    val pnlExplanation = timeChangesPnl.combine(timeChangesPnl.rows(UTPIdentifier(1), option), noChoices)
    val sumExpiryPnl = pnlExplanation.filter(_.label == "Expiry DC").map(_.scaledPnl.quantityValue.get).sum
    assertQtyEquals(sumExpiryPnl, Quantity(-5000, UOM.USD), 1e-6)
    assertTrue(pnlExplanation.filter(_.label == "Theta DC").isEmpty)
  }

  @Test
  def testSwapMidPeriodHasNoUnexplainedTerms{
    val index = Index.BRT11.copy(precision = None)
    val swap = SinglePeriodSwap(
      index,
      15.0(index.priceUOM),
      100(index.uom),
      Month(2011, 3),
      cleared = true
    )
    val marketDay1 = (4 Mar 2011).endOfDay
    val marketDay2 = (7 Mar 2011).endOfDay
    val env1 = Environment(UnitTestingAtomicEnvironment(
      marketDay1,
      {
        case _ : ForwardPriceKey => 66.0(index.priceUOM)
        case _ : IndexFixingKey => 77.0(index.priceUOM)
        case _ : DiscountRateKey => new Quantity(1.0)
      }
    )).forwardState(marketDay2).ignoreSwapRounding
    val env2 = Environment(UnitTestingAtomicEnvironment(
      marketDay2,
      {
        case _ : ForwardPriceKey => 65.0(index.priceUOM)
        case IndexFixingKey(_, day) if day == marketDay2.day => 79.0(index.priceUOM)
        case _ : IndexFixingKey => 77.0(index.priceUOM)
        case _ : DiscountRateKey => new Quantity(1.0)
      }
    )).ignoreSwapRounding
    val marketChangesPnl = new MarketChangesPnl(env1.atomicEnv, env2.atomicEnv, Map(UTPIdentifier(1) -> swap))
      val choices = ReportSpecificChoices(Map(showEqFutures_str -> false))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), swap), choices)
    val components = pnlExplanation.map(_.componentName).toSet
    // Should only have price change and fixing components - nothing unexplained
    assertEquals(components, Set("Delta DC", "Fixings DC"))
  }

  @Test
  def testSeetalBugReport_PREM_UNL_EURO_BOB_OXY_NWE_BARGES {
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES.copy(precision = None)
    val market = index.market
    val period = Month(2011, 4)
    val volume = 10000(index.uom)
    val swap = SinglePeriodSwap(index, 0(index.priceUOM), volume, period, cleared = true)

    val marketDay1 = (13 Apr 2011).endOfDay
    val marketDay2 = (14 Apr 2011).startOfDay

    val price1 = 1101.446448(index.priceUOM)
    val price2 = 1098.461042(index.priceUOM)

    val env1 = Environment(UnitTestingAtomicEnvironment(
    marketDay1, {
      case _: ForwardPriceKey => price1
      case _: IndexFixingKey => 0(index.priceUOM)
    }
    )).forwardState(marketDay2)
    val env2 = Environment(UnitTestingAtomicEnvironment(
    marketDay2, {
      case _: ForwardPriceKey => price2
      case _: IndexFixingKey => 0(index.priceUOM)
    }
    )).undiscounted

    val marketChangesPnl = new MarketChangesPnl(env1.atomicEnv, env2.atomicEnv, Map(UTPIdentifier(1) -> swap))
    val choices = ReportSpecificChoices(Map(showEqFutures_str -> false))
    val pnlExplanation = marketChangesPnl.combine(marketChangesPnl.rows(UTPIdentifier(1), swap), choices)
    val components = pnlExplanation.map(_.componentName).toSet
    // Should only have price change and fixing components - nothing unexplained
    assertEquals(components, Set("Delta DC"))

    assertEquals(pnlExplanation.size, 1)
    val explain = pnlExplanation.head
    assertQtyEquals(explain.d1Price.get.quantityValue.get, price1, 1e-9)
    assertQtyEquals(explain.priceChange.get.quantityValue.get, price2 - price1, 1e-9)
    assertQtyEquals(explain.pnl.quantityValue.get * explain.scale, -(14927.03)(USD), .01)
  }

  @Test
  def testFuturesOptionNoUnexplained {
    val md1 = Day(2010, 10, 14).endOfDay
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val month = Month(2011, 1)

    val env1Fwd = makeEnv(md1).forwardState(md2)
    val d1Fwd = env1Fwd.atomicEnv
    val dP = Quantity(3, USD / BBL)
    val env2 = makeEnv(md2, .05, dP)
    val d2 = env2.atomicEnv

    val K = env1Fwd.forwardPrice(market, month)  + dP
    val option = new FuturesOption(market, market.optionExpiry(month), month, K, Quantity(100000, BBL), Call, American)

    val utps = Map(UTPIdentifier(1) -> option)
    val marketChangesPnl = new MarketChangesPnl(d1Fwd, d2, utps)

    val choices = ReportSpecificChoices(Map[String, Any](atmVega_str -> false))

    val rows = marketChangesPnl.rows(UTPIdentifier(1), option)
    val pnlExplanation = marketChangesPnl.combine(rows, choices)
    val explanationSum = Quantity.sum(pnlExplanation.map(r => r.pnl.quantityValue.get * r.scale))

    val env1 = makeEnv(md1)
    val d1 = env1.atomicEnv
    val timeChanges = new TimeChangesPnl(Environment(d1), d2.marketDay, utps)
    val timeChangesExplanation = timeChanges.combine(timeChanges.rows(UTPIdentifier(1), option), choices)
    val timeChangesExplanationSum = Quantity.sum(timeChangesExplanation.map(r => r.pnl.quantityValue.get * r.scale))

    val totalExplained = explanationSum + timeChangesExplanationSum

    val d1Mtm = option.mtm(env1, USD)
    val d1FwdMtm = option.mtm(env1Fwd, USD)
    val d2Mtm = option.mtm(env2, USD)

    val plainPnl = d2Mtm - d1Mtm
    val plainFwdPnl = d2Mtm - d1FwdMtm

    assertQtyEquals(plainFwdPnl, explanationSum, 1e-3)
    assertQtyEquals(plainPnl - plainFwdPnl, timeChangesExplanationSum, 1e-3)
    assertQtyEquals(plainPnl, totalExplained, 1e-3)
  }
}
