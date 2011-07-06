package starling.instrument


import org.testng.annotations._
import org.testng.Assert._

import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.utils.ScalaTestUtils._
import starling.utils.QuantityTestUtils._
import starling.curves._
import starling.quantity.{Percentage, Quantity}
import starling.market.{JonTestEnv, TestMarketSpec, FuturesMarket, Market}
import starling.models.{American, European, Put, Call}
import starling.daterange.Period._
import starling.daterange._
import starling.utils.Log
import starling.utils.StarlingTest

class PnlExplanationTests extends JonTestEnv  with StarlingTest{
  @Test
  def testExplanation {
    val md1 = Day(2010, 10, 14).endOfDay
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val month = Month(2011, 5)

    val env1 = makeEnv(md1)
    val d1 = env1.atomicEnv
    val env2 = makeEnv(md2, 1, market.standardShift, market.standardShift)
    val d2 = env2.atomicEnv

    val p = env1.forwardPrice(market, month)
    val option = new FuturesOption(market, market.optionExpiry(month), month, p, Quantity(1000, BBL), Call, American)

    val explanation = option.explain(env1, env2, curveKeys => Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2)), USD)
    val volMarket = market

    explanation.foreach {
      case CurveKeyExplanation(cks, _,None,None, q, _, _, _) if cks == List(DiscountCurveKey(USD)) => assertPivotQtyEquals(q, 0.14(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(OilAtmVolCurveKey(volMarket), OilVolSkewCurveKey(volMarket)) => assertPivotQtyEquals(q, -(471.41)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(1), q, _, _, _) if cks == List(OilAtmVolCurveKey(volMarket), OilVolSkewCurveKey(volMarket)) => assertPivotQtyEquals(q, 24376.48(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(2), q, _, _, _) if cks == List(OilAtmVolCurveKey(volMarket), OilVolSkewCurveKey(volMarket)) => assertPivotQtyEquals(q, -(503.40)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, -(0.0)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 151.06(USD), 1e-2)
      case CurveKeyExplanation(cks, _, _, Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.74(USD), 1e-2)
      case m => throw new AssertionError("not expected: " + m)
    }

    val d1Mtm = option.mtm(env1, USD)
    val plainPnl = option.mtm(env2, USD) - d1Mtm
    val explainedPnl = explanation.map(_.value).sum.quantityValue.get

    assertTrue(((plainPnl - explainedPnl) / plainPnl).value < 0.003, "The difference between the explained and plain pnl should be small")
  }

  @Test
  def testCSOExplanation {
    val md1 = Day(2010, 10, 14).endOfDay
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val Month1 = Month(2011, 5)
    val Month2 = Month(2011, 6)
    val spread = Spread(Month1, Month1 + 1)

    val env1 = makeEnv(md1)
    val d1 = env1.atomicEnv
    val env2 = makeEnvShift(md2, dPrice = Map(Month1 -> Quantity(1, market.priceUOM), Month1 + 1-> Quantity(-.1, market.priceUOM)),
      dStdDev = Map(Spread(Month1, Month1 + 1) ->  Quantity(1, market.priceUOM)),
      dStdDevSkew = Map(Spread(Month1, Month1 + 1) ->  1.0))
    val d2 = env2.atomicEnv

    val p = env1.forwardPrice(market, Month1)
    val option = new SingleCalendarSpreadOption(market, market.optionExpiry(Month1), Month1, Month1 + 1, Quantity(0, market.priceUOM), Quantity(1000, BBL), Call)

    val explanation = option.explain(env1, env2, curveKeys => Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2)), USD)
    val volMarket = market

    explanation.foreach {
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(DiscountCurveKey(USD)) => assertPivotQtyEquals(q, 0.01(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 266.37(USD), 1e-2)
      case c@CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 45.98(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, (0.0)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 459.79(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, 1.23(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(1), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, 280.39(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(2), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, 1.69(USD), 1e-2)
      case m => throw new AssertionError("not expected: " + m)
    }

    // explained no longer so accurate as curvePnl - diff pnl moved to cross terms
//    val d1Mtm = option.mtm(env1, USD)
//    val plainPnl = option.mtm(env2, USD) - d1Mtm
//    val explainedPnl = explanation.map(_.value).sum.quantityValue.get
//
//    assertTrue(((plainPnl - explainedPnl) / plainPnl).value < 1e-2, "The difference between the explained and plain pnl should be small: " + (plainPnl - explainedPnl) + ", " + plainPnl)
  }

  @Test
  def testCSOPutExplanation {
    val md1 = Day(2010, 10, 14).endOfDay
    val md2 = Day(2010, 10, 15).endOfDay

    val market = Market.NYMEX_WTI
    val Month1 = Month(2011, 5)
    val Month2 = Month(2011, 6)
    val spread = Spread(Month1, Month1 + 1)

    val env1 = makeEnv(md1)
    val d1 = env1.atomicEnv
    val env2 = makeEnvShift(md2, dPrice = Map(Month1 -> Quantity(1.07, market.priceUOM), Month1 + 1-> Quantity(1.07, market.priceUOM)),
      dStdDev = Map(Spread(Month1, Month1 + 1) ->  Quantity(.1, market.priceUOM)),
      dStdDevSkew = Map(Spread(Month1, Month1 + 1) ->  1.0))
    val d2 = env2.atomicEnv

    val p = env1.forwardPrice(market, Month1)
    val option = new SingleCalendarSpreadOption(market, market.optionExpiry(Month1), Month1, Month1 + 1, Quantity(-2, market.priceUOM), Quantity(-100000, BBL), Put)

    val explanation = option.explain(env1, env2, curveKeys => Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2)), USD)
    val volMarket = market

    explanation.foreach {
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(DiscountCurveKey(USD)) => assertPivotQtyEquals(q, -.19(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case c@CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, -9767.91(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month2)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, (0.0)(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(1), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 9767.91(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(DateRangePeriod(Month1)), Some(2), q, _, _, _) if cks == List(ForwardCurveKey(market)) => assertPivotQtyEquals(q, 0.0(USD), 1e-2)
      case CurveKeyExplanation(cks, _, None, None, q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, -13908.14(USD), 1e-2) // large because we also have a skew change
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(1), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, -1063.55(USD), 1e-2)
      case CurveKeyExplanation(cks, _, Some(SpreadPeriod(month1, month2)), Some(2), q, _, _, _) if cks == List(SpreadAtmStdDevCurveKey(market), SpreadSkewStdDevCurveKey(market)) => assertPivotQtyEquals(q, -16.91(USD), 1e-2)
      case m => throw new AssertionError("not expected: " + m)
    }
    // explained no longer so accurate as curvePnl - diff pnl moved to cross terms
//    val d1Mtm = option.mtm(env1, USD)
//    val plainPnl = option.mtm(env2, USD) - d1Mtm
//    val explainedPnl = explanation.map(_.value).sum.quantityValue.get + option.theta(env1)
//
//    assertTrue(((plainPnl - explainedPnl) / plainPnl).value < 1e-2, "The difference between the explained and plain pnl should be small: " + (plainPnl - explainedPnl))
  }
}
