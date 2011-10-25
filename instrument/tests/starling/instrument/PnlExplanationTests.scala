package starling.instrument


import org.testng.annotations._
import org.testng.Assert._

import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.utils.ScalaTestUtils._
import starling.quantity.utils.QuantityTestUtils._
import starling.curves._
import starling.quantity.{Percentage, Quantity}
import starling.market.{JonTestEnv, TestMarketTest, FuturesMarket, Market}
import starling.models.{American, European, Put, Call}
import starling.daterange.Period._
import starling.daterange._
import starling.instrument.utils._
import starling.utils.{Log, StarlingTest}

class PnlExplanationTests extends JonTestEnv  with StarlingTest{
  @Test
  def testExplanation {
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val month = Month(2011, 5)

    val env1Fwd = makeEnv(md2)
    val d1Fwd = env1Fwd.atomicEnv
    val env2 = makeEnv(md2, 1, market.standardShift, market.standardShift)
    val d2 = env2.atomicEnv

    val K = env1Fwd.forwardPrice(market, month)
    val option = new FuturesOption(market, market.optionExpiry(month), month, K, Quantity(100000, BBL), Call, American)

    val environmentFor: (Set[CurveKey]) => Environment = curveKeys => Environment(OverrideForCurveKeysEnvironment(d1Fwd, curveKeys, d2))
    val explanation = option.explain(env1Fwd, env2, environmentFor, USD)
    val explainedPnlPQ = explanation.map(_.value).sum
    val explainedPnl = explanation.map(_.value).sum.quantityValue.get
    val curveKeys = AtomicDatumKeyUtils.curveKeys(option, env1Fwd, USD)
    val (crossTerms, _, unexplained) = option.components(env1Fwd, env2, environmentFor, USD, explainedPnlPQ, curveKeys)

    val volMarket = market

    val d1Mtm = option.mtm(env1Fwd, USD)
    val d2Mtm = option.mtm(env2, USD)
    val plainPnl = d2Mtm - d1Mtm

    // explained + cross terms same as the plain pnl (i.e. no unexplained)
    assertQtyEquals(plainPnl, (explainedPnlPQ + crossTerms).quantityValue.get, 1e-3)
    assertTrue(unexplained.isAlmostZero)

    explanation.foreach {
      case CurveKeyExplanation(cks, _,None,None, q, _, _, _) if cks == List(DiscountCurveKey(USD)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(OilAtmVolCurveKey(volMarket), OilVolSkewCurveKey(volMarket)) => assertPivotQtyEquals(q, -(471.41)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(1), q, _, _, _) if cks == List(OilAtmVolCurveKey(volMarket), OilVolSkewCurveKey(volMarket)) => assertPivotQtyEquals(q, 2431076.49(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(2), q, _, _, _) if cks == List(OilAtmVolCurveKey(volMarket), OilVolSkewCurveKey(volMarket)) => assertPivotQtyEquals(q, -49939.62(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, -(0.0)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 15103.10(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 73.72(USD), 1e-2)
      case m => throw new AssertionError("not expected: " + m)
    }

  }

  @Test
  def testCSOExplanation {
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val Month1 = Month(2011, 5)
    val Month2 = Month(2011, 6)

    val env1Fwd = makeEnv(md2)
    val d1 = env1Fwd.atomicEnv
    val env2 = makeEnvShift(md2, dPrice = Map(Month1 -> Quantity(1, market.priceUOM), Month1 + 1-> Quantity(-.1, market.priceUOM)),
      dStdDev = Map(SpreadPeriod(Month1, Month1 + 1) ->  Quantity(1, market.priceUOM)),
      dStdDevSkew = Map(SpreadPeriod(Month1, Month1 + 1) ->  1.0))
    val d2 = env2.atomicEnv

    val p = env1Fwd.forwardPrice(market, Month1)
    val option = new SingleCalendarSpreadOption(market, market.optionExpiry(Month1), Month1, Month1 + 1, Quantity(0, market.priceUOM), Quantity(100000, BBL), Call)

    val environmentFor: (Set[CurveKey]) => Environment = curveKeys => Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2))
    val explanation = option.explain(env1Fwd, env2, environmentFor, USD)
    val volMarket = market

    val explainedPnlPQ = explanation.map(_.value).sum
    val curveKeys = AtomicDatumKeyUtils.curveKeys(option, env1Fwd, USD)
    val (crossTerms, _, unexplained) = option.components(env1Fwd, env2, environmentFor, USD, explainedPnlPQ, curveKeys)

    val d1Mtm = option.mtm(env1Fwd, USD)
    val d2Mtm = option.mtm(env2, USD)
    val plainPnl = d2Mtm - d1Mtm

    // explained + cross terms same as the plain pnl (i.e. no unexplained)
    assertQtyEquals(plainPnl, (explainedPnlPQ + crossTerms).quantityValue.get, 1e-3)
    assertTrue(unexplained.isAlmostZero)

    explanation.foreach {
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(DiscountCurveKey(USD)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 266.37(USD), 1e-2)
      case c@CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 4595.25(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, (0.0)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 45952.49(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, 1.23(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(1), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, 27960.03(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(2), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, 170.77(USD), 1e-2)
      case m => throw new AssertionError("not expected: " + m)
    }
  }

  @Test
  def testCSOPutExplanation {
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val Month1 = Month(2011, 5)
    val Month2 = Month(2011, 6)
    val spread = Spread(Month1, Month1 + 1)

    val env1Fwd = makeEnv(md2)
    val d1 = env1Fwd.atomicEnv
    val env2 = makeEnvShift(md2, dPrice = Map(Month1 -> Quantity(1.07, market.priceUOM), Month1 + 1-> Quantity(1.07, market.priceUOM)),
      dStdDev = Map(SpreadPeriod(Month1, Month1 + 1) ->  Quantity(.1, market.priceUOM)),
      dStdDevSkew = Map(SpreadPeriod(Month1, Month1 + 1) ->  1.0))
    val d2 = env2.atomicEnv

    val p = env1Fwd.forwardPrice(market, Month1)
    val option = new SingleCalendarSpreadOption(market, market.optionExpiry(Month1), Month1, Month1 + 1, Quantity(-2, market.priceUOM), Quantity(-100000, BBL), Put)

    val environmentFor: (Set[CurveKey]) => Environment = curveKeys => Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2))
    val explanation = option.explain(env1Fwd, env2, environmentFor, USD)
    val volMarket = market

    val explainedPnlPQ = explanation.map(_.value).sum
    val curveKeys = AtomicDatumKeyUtils.curveKeys(option, env1Fwd, USD)
    val (crossTerms, _, unexplained) = option.components(env1Fwd, env2, environmentFor, USD, explainedPnlPQ, curveKeys)

    val d1Mtm = option.mtm(env1Fwd, USD)
    val d2Mtm = option.mtm(env2, USD)
    val plainPnl = d2Mtm - d1Mtm

    // explained + cross terms same as the plain pnl (i.e. no unexplained)
    assertQtyEquals(plainPnl, (explainedPnlPQ + crossTerms).quantityValue.get, 1e-3)
    assertTrue(unexplained.isAlmostZero)
    
    explanation.foreach {
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(DiscountCurveKey(USD)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)

      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 9743.87(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, -9743.87(USD), 1e-2)

      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, (0.0)(USD), 1e-2)

      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, -13908.14(USD), 1e-2) // large because we also have a skew change
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(1), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, -1059(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(2), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, -16.82(USD), 1e-2)
      case m => throw new AssertionError("not expected: " + m)
    }
  }
}
