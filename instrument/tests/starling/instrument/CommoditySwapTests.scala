package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.utils.ScalaTestUtils._
import starling.utils.QuantityTestUtils._
import starling.market._
import rules.{Precision, CommonPricingRule}
import starling.curves._
import starling.market.formula._
import starling.daterange._
import starling.daterange.Day._
import starling.quantity.RichQuantity._
import starling.pivot.PivotQuantity
import starling.utils.{AtomicDatumKeyUtils, StarlingTest}
import starling.quantity.{Conversions, Quantity}
import starling.varcalculator.{ForwardPriceRiskFactor, RiskFactorUtils}
import java.util.Random
import starling.models.DefaultRiskParameters

class CommoditySwapTests extends JonTestEnv {

  @Test
  def testMTM {
    val marketDayAndTime = Day(2009, 9, 15).startOfDay
    val forwardPrice = Quantity(200, USD / MT)
    val historicPrice = Quantity(100, USD / MT)
    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _: ForwardPriceKey => forwardPrice
          case _: FixingKey => historicPrice
          case DiscountRateKey(_, day, _) => scala.math.exp(-0.05 * day.daysSinceInYears(marketDay.day))
        }

        def marketDay = marketDayAndTime
      }
    ).ignoreSwapRounding
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market
    val sep09 = Month(2009, 9)
    val volume = Quantity(100, MT)
    val strike = Quantity(200, USD / MT)

    val swap = new SingleCommoditySwap(
      index,
      strike,
      volume,
      sep09,
      false
    )

    val mtm = swap.mtm(env)

    val observationDays = sep09.days.filter(market.isObservationDay(_)).toList
    val fixingDays = observationDays.filter(_.endOfDay <= marketDayAndTime)
    val forwardDays = observationDays filterNot (fixingDays contains)
    val swapPrice = (historicPrice * fixingDays.size + forwardPrice * forwardDays.size) / observationDays.size
    val discount = env.discount(USD, CommoditySwap.swapSettlementDate(sep09.firstDay))
    val expectedMTM = (swapPrice - strike) * volume * discount
    assertQtyEquals(mtm, expectedMTM, 1e-6)
  }

  @Test
  def testDelta {
    val marketDayAndTime = Day(2009, 9, 15).startOfDay
    val forwardPrice = Quantity(200, USD / BBL)
    val historicPrice = Quantity(100, USD / BBL)
    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(_, d, _) => {
            forwardPrice
          }
          case _: FixingKey => historicPrice
          case DiscountRateKey(_, day, _) => scala.math.exp(-0.05 * day.daysSinceInYears(marketDay.day))
        }

        def marketDay = marketDayAndTime
      }
    )

    val index = Index.BRT11
    val market = index.market

    val period = Month(2009, 12)
    val volume = Quantity(100, BBL)
    val strike = Quantity(200, USD / BBL)

    val swap = new SingleCommoditySwap(
      index,
      strike,
      volume,
      period,
      false
    )

    val mtm = swap.mtm(env)

    val observationDays = period.days.filter(market.isObservationDay(_)).toList
    val fixingDays = observationDays.filter(_.endOfDay <= marketDayAndTime)
    val forwardDays = observationDays filterNot (fixingDays contains)
    val swapPrice = (historicPrice * fixingDays.size + forwardPrice * forwardDays.size) / observationDays.size
    val discount = env.discount(USD, CommoditySwap.swapSettlementDate(period.firstDay))
    val expectedMTM = (swapPrice - strike) * volume * discount
    assertQtyEquals(mtm, expectedMTM, 1e-6)

    val rfs = swap.riskFactors(env, USD).toArray
    val delta1 = swap.riskFactorDerivative(env, rfs(0), USD)
    val position1 = swap.riskFactorPosition(env, rfs(0), USD)
    val delta2 = swap.riskFactorDerivative(env, rfs(1), USD)
    val position2 = swap.riskFactorPosition(env, rfs(1), USD)
  }

  @Test
  def testSpreadIndexSwap {
    val spreadIndex = Index.IPE_GAS_OIL_VS_IPE_BRENT
    val index1 = spreadIndex.indexes.head
    val index2 = spreadIndex.indexes.tail.head
    val market1 = index1.market
    val market2 = index2.market

    val env = Environment(new TestingAtomicEnvironment() {
      def marketDay = Day(2009, 9, 15).startOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case ForwardPriceKey(`market1`, _, _) => Quantity(200, USD / MT)
        case ForwardPriceKey(`market2`, _, _) => Quantity(250, USD / BBL)
        case FixingKey(`index1`, _) => Quantity(100, USD / MT)
        case FixingKey(`index2`, _) => Quantity(150, USD / BBL)
        case DiscountRateKey(_, day, _) => scala.math.exp(-0.05 * day.daysSinceInYears(marketDay.day))
      }
    })

    val sep09 = Month(2009, 9)
    val volume = Quantity(20, BBL)
    val strike = Quantity(0, USD / BBL)

    val swapSingle1 = new SingleCommoditySwap(
      index1,
      Quantity(0, USD / MT),
      index1.market.convert(volume, MT).get,
      sep09,
      false
    )
    val swapSingle2 = new SingleCommoditySwap(
      index2,
      strike,
      volume,
      sep09,
      false
    )
    val swapSpread = new SingleCommoditySwap(
      spreadIndex,
      strike,
      volume,
      sep09,
      false,
      pricingRule = CommonPricingRule
    )

    val swapSingle1Mtm = swapSingle1.mtm(env)
    val swapSingle2Mtm = swapSingle2.mtm(env)
    val swapSpreadMtm = swapSpread.mtm(env)

    // because rounding is different for the gas oil crack index compared to the single indices value is slightly off
    assertQtyEquals(swapSpreadMtm, swapSingle1Mtm - swapSingle2Mtm, .1)
  }

  /**test that a strip of swaps over two months has the same value as two monthly swaps
   */
  @Test
  def testMonthDecomposition {
    val marketDayAndTime = Day(2009, 9, 15).startOfDay
    val forwardPrice = Quantity(200, USD / MT)
    val historicPrice = Quantity(100, USD / MT)

    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _: ForwardPriceKey => forwardPrice
          case _: FixingKey => historicPrice
          case DiscountRateKey(_, day, _) => scala.math.exp(-0.05 * day.daysSinceInYears(marketDay.day))
        }

        def marketDay = marketDayAndTime
      }
    )
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market
    val sep09 = Month(2009, 9)
    val oct09 = sep09 + 1
    val volume = Quantity(100, MT)
    val strike = Quantity(200, USD / MT)

    val swapSep09 = new CommoditySwap(
      index,
      strike,
      volume,
      sep09,
      false
    )
    val swapOct09 = new CommoditySwap(
      index,
      strike,
      volume,
      oct09,
      false
    )
    val swapSeptoOct09 = new CommoditySwap(
      index,
      strike,
      volume,
      StripPeriod(sep09, oct09),
      false
    )

    assertQtyEquals(
      swapSep09.asUtpPortfolio(Day(2009, 1, 1)).mtm(env) + swapOct09.asUtpPortfolio(Day(2009, 1, 1)).mtm(env),
      swapSeptoOct09.asUtpPortfolio(Day(2009, 1, 1)).mtm(env),
      1e-6
    )
  }

  @Test
  def testC3651761 {
    val marketDayAndTime = Day(2011, 3, 16).startOfDay
    val frbob = Quantity(2.994, USD / GAL)
    val funl = Quantity(2.9, USD / GAL)

    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(Market.NYMEX_GASOLINE, _, _) => frbob
          case ForwardPriceKey(Index.UNL_87_USGC_PIPELINE, _, _) => funl
        }

        def marketDay = marketDayAndTime
      }
    )

    val index = new FormulaIndex("unl vs rbob", Formula("MKT(34) - MKT(933)"), USD, GAL, None, None, None)
    val volume = Quantity(10500000, GAL)
    val strike = Quantity(-0.095, USD / GAL)
    val cs = new CommoditySwap(index, strike, volume, Month(2011, 4), true, CommonPricingRule)
    val fixed = funl - frbob
    assertQtyEquals(Quantity(10500, USD), (fixed - strike) * volume, 1e-6)
    assertQtyEquals(cs.asUtpPortfolio(Day(2011, 3, 14)).mtm(env), (fixed - strike) * volume, 1e-6)
  }

  @Test
  def testUndiscountedMTM {
    val market = Index.WTI10
    val period = Month(2011, 5)
    val strike = Quantity(80, USD / BBL)
    val volume = Quantity(1000, BBL)
    val swap1 = new CommoditySwap(market, strike, volume, period, true)
    val swap2 = new SingleCommoditySwap(market, strike, volume, period, true)
    val env = makeEnv(Day(2010, 12, 1).endOfDay).ignoreSwapRounding
    val price = env.averagePrice(market, period)
    val discount = env.discount(USD, CommoditySwap.swapSettlementDate(period.firstDay))
    assertQtyEquals((price - strike) * volume, swap1.asUtpPortfolio(Day(2009, 1, 1)).mtm(env))
    assertQtyEquals((price - strike) * volume, swap2.mtm(env))
  }

  @Test
  def testFormulaIndex {
    val formulaIndex = new FormulaIndex("WTI vs RBOB", Formula("MKT(7) - MKT(933)"), USD, BBL, None, None, None)
    val rbob = Index.RBOB10
    val wti = Index.WTI10

    val period = Month(2011, 5)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(1000, BBL)
    val marketDay = Day(2010, 12, 1).endOfDay
    val env = makeEnv(marketDay).ignoreSwapRounding
    val price1 = env.averagePrice(formulaIndex, period, CommonPricingRule, formulaIndex.priceUOM)
    val swap1 = new SingleCommoditySwap(formulaIndex, strike, volume, period, true, pricingRule = CommonPricingRule)
    val tradeDay = Day(2009, 1, 1)
    val mtm1 = swap1.mtm(env)
    assertQtyEquals((price1 - strike) * volume, mtm1)

    val strikeGal = rbob.convert(Quantity(2, USD / BBL), rbob.priceUOM).get
    val volumeGal = rbob.convert(Quantity(1000, BBL), rbob.uom).get
    val swap2 = new CommoditySwap(wti, strike, volume, period, true, pricingRule = CommonPricingRule)
    val swap3 = new CommoditySwap(rbob, Quantity(0, USD / GAL), -volumeGal, period, true, pricingRule = CommonPricingRule)
    val price2 = env.averagePrice(wti, period)
    val price3 = env.averagePrice(rbob, period)
    val mtm2 = swap2.asUtpPortfolio(tradeDay).mtm(env)
    val mtm3 = swap3.asUtpPortfolio(tradeDay).mtm(env)

    val price3BBL = rbob.convert(price3, USD / BBL).get
    assertQtyEquals(price1, (price2 - price3BBL), 1e-7)
    assertQtyEquals(mtm1, mtm2 + mtm3, 1e-7)

    val priceKeys = Set() ++ rbob.observationDays(period).map(SwapPrice(rbob, _)) ++ wti.observationDays(period).map(SwapPrice(wti, _))

    val diff1 = swap1.priceAndVolKeys(marketDay)._1 -- priceKeys
    val diff2 = priceKeys.filterNot(swap1.priceAndVolKeys(marketDay)._1.toSet.contains)

    assertEquals(swap1.priceAndVolKeys(marketDay), (priceKeys, Set.empty))
  }

  @Test
  def testFormulaIndexUTPs {
    val formula = new Formula("MKT(7) - MKT(28)")
    val formulaIndex = new FormulaIndex("RBOB vs Brent", formula, USD, BBL, None, None, None)
    val rbob = Index.RBOB10
    val brent = Index.BRT11

    val marketDay = Day(2010, 12, 1).endOfDay
    val env = makeEnv(marketDay)
    val period = Month(2011, 5)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(1000, BBL)
    val swap1 = new CommoditySwap(formulaIndex, strike, volume, period, true, pricingRule = CommonPricingRule)
    val utpPortfolio = swap1.asUtpPortfolio(env.marketDay.day)
    assertTrue(utpPortfolio.position(env, SwapPrice(brent, 30 May 2011)).isZero)
    assertFalse(utpPortfolio.position(env, SwapPrice(brent, 27 May 2011)).isZero)
  }

  @Test
  def testSwapPositionMidPricing {
    val index = Index.WTI10
    val period = Month(2011, 4)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(1000, BBL)
    val cs = SingleCommoditySwap(index, strike, volume, period, true)

    for (marketDay <- Day(2011, 3, 15) until Day(2011, 5, 10); timeOfDay <- List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay)) {
      val marketDayAndTime = marketDay.atTimeOfDay(timeOfDay)
      val env = makeEnv(marketDayAndTime)

      val position = cs.position(env, SwapPrice(index, period))

      val obDays = index.observationDays(cs.averagingPeriod)
      val liveDays = cs.liveAveragingDays(marketDayAndTime).toList
      val fixedDays = obDays.filter(_.endOfDay <= marketDayAndTime)
      assertEquals(liveDays, obDays.filter(_.endOfDay > marketDayAndTime))
      assertEquals(fixedDays ::: liveDays, obDays)

      assertQtyEquals(position, volume * (liveDays.size.toDouble / obDays.size.toDouble), 1e-8)
    }
  }

  @Test
  def testSwapBOMPositionMidPricing {
    val index = Index.WTI10
    val period = BOM(14 Apr 2011)
    val apr = Month(2011, 4)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(1000, BBL)
    val cs = SingleCommoditySwap(index, strike, volume, period, true)

    for (marketDay <- apr.days; timeOfDay <- List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay)) {
      val marketDayAndTime = marketDay.atTimeOfDay(timeOfDay)
      val env = makeEnv(marketDayAndTime)

      val position = cs.position(env, SwapPrice(index, period))

      val obDays = index.observationDays(cs.averagingPeriod)
      val liveDays = cs.liveAveragingDays(marketDayAndTime).toList
      val fixedDays = obDays.filter(_.endOfDay <= marketDayAndTime)
      assertEquals(liveDays, obDays.filter(_.endOfDay > marketDayAndTime))
      assertEquals(fixedDays ::: liveDays, obDays)

      assertQtyEquals(position, volume * (liveDays.size.toDouble / obDays.size.toDouble), 1e-8)

      if(marketDayAndTime < period.firstDay.endOfDay) {
        assertQtyEquals(position, volume)
      }

      if(marketDayAndTime >= period.lastDay.endOfDay) {
        assertQtyEquals(position, Quantity(0, BBL))
      }
    }
  }

  @Test
  def testPrice {
    val index = Index.WTI10
    val period = Month(2011, 4)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(1000, BBL)
    val cs = SingleCommoditySwap(index, strike, volume, period, true)

    for (marketDay <- Day(2011, 3, 15) until Day(2011, 5, 10); timeOfDay <- List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay)) {
      val marketDayAndTime = marketDay.atTimeOfDay(timeOfDay)

      val env = makeEnv(marketDayAndTime)
      val liveDays = cs.liveAveragingDays(marketDayAndTime).toList
      val expectedPrice = if (!liveDays.isEmpty) {
        Quantity.average(liveDays.map(d => env.forwardPrice(index.market, index.observedPeriod(d))))
      } else {
        Quantity(0, USD / BBL)
      }
      val price = cs.price(env)
      assertQtyEquals(price, expectedPrice)
    }
  }

  @Test
  def testMonthlyPositionWhenPeriodSpansMonths {
    val index = Index.DATED_BRENT
    val period = DateRange(24 Feb 2011, 2 Mar 2011)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(1030000, BBL)
    val cs = CommoditySwap(index, strike, volume, period, true).asUtpPortfolio(Day(2011, 1, 1))

    val marketDayAndTime = (7 Feb 2011).endOfDay
    val env = makeEnv(marketDayAndTime)

    val febSens = SwapPrice(index, Month(2011, 2))
    val marSens = SwapPrice(index, Month(2011, 3))

    val febPos = cs.position(env, febSens)
    val marPos = cs.position(env, marSens)

    assertQtyEquals(febPos, 618000(BBL), 1e-7)
    assertQtyEquals(marPos, 412000(BBL), 1e-7)
  }

  @Test
  def testPositionWhenBrentInMT {
    val index = Index.BRT11
    val period = Month(2012, 1)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(16000, MT)
    val cs = CommoditySwap(index, strike, volume, period, true).asUtpPortfolio(Day(2011, 1, 1))

    val marketDayAndTime = (7 Feb 2011).endOfDay
    val env = makeEnv(marketDayAndTime)

    val janSens = SwapPrice(index, Month(2012, 1))

    val janPos = cs.position(env, janSens)
    val mtPos = janPos / Quantity(7.57, BBL/MT)
    assertQtyEquals(mtPos, volume, 1e-7)
  }

  @Test
  def testMogasVsBrentCanBeInMT {
    // Seetal can enter the trade in either MT or BBL
    val mogas = Index.MOGAS_95_UNL_10PPM_NWE_BARGES
    val brent = Index.BRT11
    val index = Index.MOGAS_95_UNL_10PPM_NWE_BARGES_VS_IPE_BRENT

    val period = Month(2011, 10)
    val env = Environment(new UnitTestingAtomicEnvironment(DayAndTime(1 Jan 2011, TimeOfDay.EndOfDay), {
      key => key match {
        case ForwardPriceKey(market, _, _) => Quantity(10, market.priceUOM)
      }
    }))

    val strike1 = Quantity(10, USD / BBL)
    val volume1 = Quantity(1000, BBL)

    val cs1 = new CommoditySwap(index, strike1, volume1, period, cleared = true, CommonPricingRule)
    val mtm1 = cs1.asUtpPortfolio(2 Jan 2011).mtm(env)
    val scsA1 = new SingleCommoditySwap(mogas, strike1, volume1, period, cleared = true)
    val scsA2 = new SingleCommoditySwap(brent, strike1.copy(value = 0.0), -volume1, period, cleared = true)
    assertQtyEquals(mtm1, scsA1.mtm(env) + scsA2.mtm(env), 1e-7)

    val strike2 = Quantity(10, USD / MT)
    val volume2 = Quantity(1000, MT)

    val cs2 = new CommoditySwap(index, strike2, volume2, period, cleared = true, CommonPricingRule)
    val mtm2 = cs2.asUtpPortfolio(2 Jan 2011).mtm(env)
    val scsB1 = new SingleCommoditySwap(mogas, strike2, volume2, period, cleared = true)
    val scsB2 = new SingleCommoditySwap(brent, strike2.copy(value = 0.0), -volume2, period, cleared = true)
    assertQtyEquals(mtm2, scsB1.mtm(env) + scsB2.mtm(env), 1e-7)
  }

  @Test
  def testHedgingInstrumentForCommonSwap {
    // wti has a holiday in jan but brent doesn't. because we're using common pricing it means the swap will
    // have less sensitivity (one day less) to brent than a normal swap would. So the hedging instrument will
    // not be a perfect match for one leg of the trade.
    val index = Index.NYMEX_WTI_VS_IPE_BRENT
    val wti = Index.WTI10
    val brent = Index.BRT11

    val period = Month(2011, 1)
    val strike = Quantity(1, USD / BBL)
    val volume = Quantity(-1000, BBL)
    val cs = CommoditySwap(index, strike, volume, period, true, CommonPricingRule).asUtpPortfolio(Day(2010, 1, 1))
    val brentHedge = CommoditySwap(brent, strike, volume, period, true).asUtpPortfolio(Day(2010, 1, 1))

    val brentSens = SwapPrice(brent, Month(2011, 1))

    def pos(md: DayAndTime): Quantity = {
      val env = makeEnv(md)
      val brentPos = cs.position(env, brentSens)
      brentPos
      cs.firstOrderDerivative(env, brentSens, USD)
    }

    val janHoliday = 17 Jan 2011
    assertTrue(!wti.isObservationDay(janHoliday))
    assertTrue(brent.isObservationDay(janHoliday))

    val brentHedgeObDays = brent.observationDays(period)
    val brentObDays = brentHedgeObDays.filterNot(_ == janHoliday)

    val dailyHedgePos = volume / brentHedgeObDays.size
    val dailyPos = volume / brentObDays.size

    for (marketDay <- ((30 Dec 2010) until (1 Feb 2011)).filter(_.isWeekday); timeOfDay <- List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay)) {
      val marketDayAndTime = marketDay.atTimeOfDay(timeOfDay)
      val env = makeEnv(marketDayAndTime)
      val pos = cs.position(env, brentSens)
      val hedgePos = brentHedge.position(env, brentSens)

      val fixedHedgeDays = brentHedgeObDays.filter(_.endOfDay <= marketDayAndTime)
      val fixedDays = brentObDays.filter(_.endOfDay <= marketDayAndTime)

      val calculedHedgePos = volume - (dailyHedgePos * fixedHedgeDays.size)
      val calculedPos = -(volume - (dailyPos * fixedDays.size))

      val overallPosition = pos + hedgePos
      assertQtyEquals(overallPosition, calculedPos + calculedHedgePos, 1e-8)

      // before the holiday, and during the fixing, we should be short overall because we have hedged with an instrument
      // that observes more days. So as each day fixes our position decreases less in the hedge than it does in our trade.
      // So the short overall position will increase all the way up to the holiday day. Then at the end of the holiday more
      // days have fixed for the hedge than have for the trade so the overall position then becomes long.
      if (marketDayAndTime < janHoliday.endOfDay) {
        assertTrue(overallPosition.isNegativeOrZero, "Failed: " + (marketDayAndTime, overallPosition))
      } else if (marketDayAndTime >= janHoliday.endOfDay) {
        assertTrue(overallPosition.isPositve, "Failed: " + (marketDayAndTime, overallPosition))
      }
    }
  }

  @Test
  def testPublishedIndexPosition {
    val marketDay = (5 Jan 2011).endOfDay
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val period = Month(2011, 10)
    val volume = 100(index.uom)
    val swap = SingleCommoditySwap(index, 50(index.priceUOM), volume, period, cleared = true)
    val z = 0.1
    val env = Environment(UnitTestingAtomicEnvironment(
    marketDay, {
      case _: ForwardPriceKey => 70(index.priceUOM)
      case DiscountRateKey(_, day, _) => math.exp(-z * (day.daysSinceInYears(marketDay.day)))
    }
    ))
    assertQtyEquals(volume, swap.position(env, PriceDifferentiable(index.market, period)), 1e-6)
    assertQtyEquals(volume, swap.position(env, SwapPrice(index, period)), 1e-6)
  }

  @Test
  def testPREM_UNL_EURO_BOB_OXY_NWE_BARGES {
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market
    val period = Month(2011, 4)
    val volume = 10000(index.uom)
    val swap = SingleCommoditySwap(index, 0(index.priceUOM), volume, period, cleared = true)

    def make(md: DayAndTime, price: Quantity) = Environment(UnitTestingAtomicEnvironment(
    md, {
      case ForwardPriceKey(`market`, d, _) => {
        val outPrice = ForwardCurve(market, md, Map(md.day -> price.value)).price(d)
        outPrice
      }
      case _: FixingKey => Quantity(0, USD / MT)
    }
    )).undiscounted

    val marketDay1 = (13 Apr 2011).endOfDay
    val marketDay2 = (14 Apr 2011).startOfDay
    val env1 = make(marketDay1, Quantity(1101.446448, USD / MT)).ignoreSwapRounding
    val env2 = make(marketDay2, Quantity(1098.461042, USD / MT)).ignoreSwapRounding

    val d1 = env1.atomicEnv
    val d2 = env2.atomicEnv

    val curveKeys = AtomicDatumKeyUtils.curveKeys(swap, d1.marketDay, USD)

    def environmentFor(curveKeys: Set[CurveKey]) = Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2)).ignoreSwapRounding
    val explanation = swap.explain(env1, env2, environmentFor, USD)

    val explainedTotal = explanation.map {
      _.value
    }.toList.sum
    println("et: " + explainedTotal)

    val allCurveKeysMtm = PivotQuantity.calcOrCatch({
      swap.mtm(environmentFor(curveKeys), USD)
    })
    val d1MTM = PivotQuantity.calcOrCatch({
      swap.mtm(env1)
    })

    val allCurvesPnl = allCurveKeysMtm - d1MTM

    val (crossTerms, rounding, unexplained) = swap.components(env1, env2, environmentFor, USD, explainedTotal, curveKeys)

    assertQtyEquals(rounding.quantityValue.get, 0(USD), 1e-5)
    assertQtyEquals(unexplained.quantityValue.get, 0(USD), 1e-5)
    assertQtyEquals(explainedTotal.quantityValue.get, -(14927)(USD), .1)
  }

  @Test
  def testBrentExplainRounding {
    val index = new FuturesFrontPeriodIndex("brent 1st", None, Market.ICE_BRENT, 0, 1, Some(Precision(2, 2)))
    val market = index.market
    val period = Month(2011, 8)

    val volume = 10000(index.uom)
    val swap = SingleCommoditySwap(index, 0(index.priceUOM), volume, period, cleared = true)

    def make(md: DayAndTime, price: Quantity, fixing: Quantity) = Environment(UnitTestingAtomicEnvironment(
    md, {
      case ForwardPriceKey(`market`, d, _) => {
        price
      }
      case FixingKey(_, d) => fixing
    }
    )).undiscounted

    val marketDay1 = (13 Apr 2011).endOfDay
    val marketDay2 = (14 Apr 2011).endOfDay
    val f1 = Quantity(103.3333, USD / BBL)
    val env1 = make(marketDay1, f1, Quantity(102.231, USD/BBL))
    val f2 = Quantity(110.5555, USD / BBL)
    val env2 = make(marketDay2, f2, Quantity(103.321, USD/BBL))

    val d1 = env1.atomicEnv
    val d2 = env2.atomicEnv

    val d2MTM = PivotQuantity.calcOrCatch({
      swap.mtm(env2)
    })

    val curveKeys = AtomicDatumKeyUtils.curveKeys(swap, d1.marketDay, USD)

    def environmentFor(curveKeys: Set[CurveKey]) = Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2))
    val explanation = swap.explain(env1, env2, environmentFor, USD)

    val explainedTotal = explanation.map {
      _.value
    }.toList.sum
    println("et: " + explainedTotal)


    val env1NoRounding = env1.copy(environmentParameters = DefaultRiskParameters)
    val env2NoRounding = env2.copy(environmentParameters = DefaultRiskParameters)

    val noRoundingPnl = PivotQuantity.calcOrCatch(swap.mtm(env2NoRounding) - swap.mtm(env1NoRounding))
    val plainPnl = PivotQuantity.calcOrCatch(swap.mtm(env2) - swap.mtm(env1))

    val (crossTerms, rounding, unexplained) = swap.components(env1, env2, environmentFor, USD, explainedTotal, curveKeys)

    assertQtyEquals((noRoundingPnl  + rounding).quantityValue.get, plainPnl.quantityValue.get, 1e-5)
    assertQtyEquals(crossTerms.quantityValue.get, 0(USD), 1e-5)
    assertQtyEquals(unexplained.quantityValue.get, 0(USD), 1e-5)
  }

  @Test
  def testPREM_UNL_EURO_BOB_OXY_NWE_BARGES__vs__Brent {
    // volume in MT, strike in USD/BBL
    val formula = new Formula("MKT(" + Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES.eaiQuoteID.get + ")- MKT(" + Index.BRT11.eaiQuoteID.get + ")")
    val c = new Conversions(Map(BBL / MT -> 8.33))
    val index = new FormulaIndex("Prem Unl Euro-Bob Oxy NWE Barges (Argus) vs IPE Brent", formula, USD, BBL, None, Some(c), None)
    val oxy = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val brent = Market.ICE_BRENT

    val oct = Month(2011, 10)
    val period = oct
    val volume = -(1000)(MT)
    val volumeBBL = index.convertUOM(volume, BBL)

    val K = 4.95(USD / BBL)
    val swap = CommoditySwap(index, K, volume, period, cleared = true, pricingRule = CommonPricingRule)

    val fOxy = 117.17(USD / BBL)
    val fBrent = 112.23(USD / BBL)
    def make(md: DayAndTime) = Environment(UnitTestingAtomicEnvironment(
    md, {
      case ForwardPriceKey(`oxy`, _, _) => {
        fOxy
      }
      case ForwardPriceKey(`brent`, _, _) => {
        fBrent
      }
    }
    )).undiscounted

    val day = 10 May 2011
    val marketDay1 = day.startOfDay
    val env1 = make(marketDay1).ignoreSwapRounding

    val pr = index.averagePrice(env1, oct, CommonPricingRule, USD / BBL)
    assertQtyEquals(pr, fOxy - fBrent, 1e-6)

    val mtmActual = ((pr - K) * (index.convert(volume, BBL).get))

    val mtm = swap.asUtpPortfolio(day).mtm(env1)
    assertQtyEquals(mtm, mtmActual, 1e-6)

    // here create two single swaps and make sure they value the same
    val iOxy = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val s1 = new SingleCommoditySwap(iOxy, 0.0(USD/BBL), volumeBBL, period, cleared = true)
    val s2 = new SingleCommoditySwap(Index.BRT11, -K, -volumeBBL, period, cleared = true)

    val s1mtm = s1.mtm(env1)
    assertQtyEquals(s1mtm, fOxy * volumeBBL, 1e-5)
    val s2mtm = s2.mtm(env1)
    assertQtyEquals(s2mtm, -volumeBBL * (fBrent - -K), 1e-5)

    assertQtyEquals(mtm, s1mtm + s2mtm, 1e-5)
  }

  @Test
  def testPnLAndRounding {
    val marketDayAndTime = Day(2011, 4, 18).startOfDay
    val index = Index.WTI10

    val market = index.market
    val fixings = Map(
      Day(2011, 4, 1) -> 118.7,
      Day(2011, 4, 4) -> 121.06,
      Day(2011, 4, 5) -> 122.22,
      Day(2011, 4, 6) -> 122.3,
      Day(2011, 4, 7) -> 122.67,
      Day(2011, 4, 8) -> 126.65,
      Day(2011, 4, 11) -> 123.98,
      Day(2011, 4, 12) -> 120.92,
      Day(2011, 4, 13) -> 122.88,
      Day(2011, 4, 14) -> 122.0,
      Day(2011, 4, 15) -> 123.45
    ).mapValues(Quantity(_, USD / BBL))

    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(market, _, _) => Quantity(123.45, USD / BBL)
          case FixingKey(`index`, day) => {
            fixings(day)
          }
        }

        def marketDay = marketDayAndTime
      }
    ).undiscounted

    val period = Month(2011, 4)
    val volume = Quantity(16000, BBL)
    val strike = Quantity(112.2, USD / BBL)

    val cs = new CommoditySwap(index, strike, volume, period, true)

    val rounding = index.precision.map(_.clearPort)
    val avg = env.averagePrice(index, period, rounding)

    val mtm = cs.asUtpPortfolio(Day(2011, 4, 15)).mtm(env)

    assertQtyEquals(mtm, Quantity(171040, USD), 1e-5)
  }

  @Test
  def testDelta_PREM_UNL_EURO_BOB_OXY_NWE_BARGES__vs__Rbob {
    val marketDayAndTime = Day(2009, 9, 15).startOfDay
    val env = Environment(
      new TestingAtomicEnvironment() {
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case ForwardPriceKey(m, d, _) => {
            200 (m.priceUOM)
          }
        }

        def marketDay = marketDayAndTime
      }
    ).undiscounted

    val c = new Conversions(Map(BBL / MT -> 8.33))
    val formula = new Formula("MKT(933)- MKT(1312)")
    val index = new FormulaIndex("NYMEX RBOB vs Prem Unl Euro-Bob Oxy NWE Barges (Argus)", formula, USD, GAL, None, None, None)
    val oxy = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val rbob = Market.NYMEX_GASOLINE

    val oct = Month(2011, 10)
    val period = oct
    val volume = -(6000)(MT)
    val strike = -(4)(USD / GAL)

    val swap = new CommoditySwap(
      index,
      strike,
      volume,
      period,
      false,
      CommonPricingRule
    )

    val s = swap.asUtpPortfolio(marketDayAndTime.day)
    val rfs = s.riskFactors(env, USD).toArray
    val basd = RiskFactorUtils.bucketRiskFactors(marketDayAndTime, rfs.toSet).flatMap {
      case f:ForwardPriceRiskFactor => Some(f)
      case _ => throw new Exception("???")
    }
    val brfs = basd.toArray
    val delta1 = s.riskFactorDerivative(env, brfs(0), USD)
    val delta2 = s.riskFactorDerivative(env, brfs(1), USD)
    val delta3 = s.riskFactorDerivative(env, brfs(2), USD)

    assertQtyEquals(delta1, -volume)
    assertQtyEquals(delta2 + delta3, rbob.convertUOM(volume, GAL), 1e-4)
  }
}
