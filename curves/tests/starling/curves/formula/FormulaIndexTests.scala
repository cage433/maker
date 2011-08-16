package starling.curves
package formula

import starling.utils.StarlingTest
import starling.market.formula._
import starling.calendar.BusinessCalendar
import starling.quantity.UOM._
import starling.market.Market._
import starling.daterange.{Month, Day}
import starling.daterange.Day._
import starling.utils.QuantityTestUtils._
import starling.market.Index._
import org.testng.annotations.Test
import org.testng.Assert._
import starling.market._
import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import collection.immutable.List
import rules.{NoPricingRule, NonCommonPricingRule, CommonPricingRule}
import starling.quantity.{Conversions, Quantity}

class FormulaIndexTests extends TestMarketSpec with ShouldMatchers {

  val env = Environment(
    new TestingAtomicEnvironment() {
      def marketDay = Day(2010, 2, 20).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case ForwardPriceKey(NYMEX_WTI, Month(2010, 3), _) => Quantity(5, USD / BBL)
        case ForwardPriceKey(ICE_BRENT, Month(2010, 3), _) => Quantity(8, USD / BBL)
        case ForwardPriceKey(ICE_GAS_OIL, Month(2010, 3), _) => Quantity(12, USD / BBL)

        case ForwardPriceKey(NYMEX_WTI, Month(2010, 4), _) => Quantity(10, USD / BBL)
        case ForwardPriceKey(ICE_BRENT, Month(2010, 4), _) => Quantity(15, USD / BBL)
        case ForwardPriceKey(ICE_GAS_OIL, Month(2010, 4), _) => Quantity(18, USD / BBL)

        case FixingKey(`WTI10`, day) => assert(WTI10.isObservationDay(day)); Quantity(8, USD / BBL)
        case FixingKey(`BRT11`, day) => assert(BRT11.isObservationDay(day)); Quantity(12, USD / BBL)
        case FixingKey(`GO11`, day) => assert(GO11.isObservationDay(day)); Quantity(15, USD / BBL)
      }
    }
  )

  @Test
  def testSimpleNonCommon {
    val index = FormulaIndex("WTI vs BRT11", Formula("1.2 * MKT(7) - 0.8*MKT(28)"), USD, BBL, None, None, None)
    index.verify

    val index1 = WTI10
    val index2 = BRT11

    val rule = NonCommonPricingRule
    val period = Month(2010, 2)

    val forwardPrice = index.averagePrice(env, period, rule, index.priceUOM)
    assertEquals(env.averagePrice(index, period, rule, index.priceUOM), forwardPrice)

    assertNotSame(rule.observationDays(index.calendars, period), index1.observationDays(period))
    assertEquals(rule.observationDays(index.calendars, period), index2.observationDays(period))

    val avg1 = Quantity.average(index1.observationDays(period).map(env.fixingOrForwardPrice(index1, _)))
    val avg2 = Quantity.average(index2.observationDays(period).map(env.fixingOrForwardPrice(index2, _)))

    assertQtyEquals(forwardPrice, avg1 * 1.2 - avg2 * 0.8)
  }

  @Test
  def testSimple {
    val index = FormulaIndex("WTI vs BRT11", Formula("1.2 * MKT(7) - 0.8*MKT(28)"), USD, BBL, None, None, None)
    index.verify

    val index1 = WTI10
    val index2 = BRT11

    val rule = CommonPricingRule
    val period = Month(2010, 2)

    val forwardPrice = index.averagePrice(env, period, rule, index.priceUOM)

    val avg1 = Quantity.average(rule.observationDays(index.calendars, period).map(env.fixingOrForwardPrice(index1, _)))
    val avg2 = Quantity.average(rule.observationDays(index.calendars, period).map(env.fixingOrForwardPrice(index2, _)))

    val commonObDays = index1.observationDays(period).intersect(index2.observationDays(period))
    assertEquals(Quantity.average(commonObDays.map(d => env.fixingOrForwardPrice(index1, d))), avg1)
    assertEquals(Quantity.average(commonObDays.map(d => env.fixingOrForwardPrice(index2, d))), avg2)

    assertQtyEquals(forwardPrice, avg1 * 1.2 - avg2 * 0.8)
  }

  @Test
  def testComplex {
    val index = FormulaIndex("0.5 WTI - 0.4 BRT11 - 1.2 GO", Formula("0.5*MKT(7) - 0.4*MKT(28) - 1.2*MKT(58)"), USD, BBL, None, None, None)
    index.verify

    val index1 = WTI10
    val index2 = BRT11
    val index3 = GO11

    val rule = CommonPricingRule
    val period = Month(2010, 2)
    val forwardPrice = index.averagePrice(env, period, rule, index.priceUOM)

    val avg1 = Quantity.average(rule.observationDays(index.calendars, period).map(env.fixingOrForwardPrice(index1, _)))
    val avg2 = Quantity.average(rule.observationDays(index.calendars, period).map(env.fixingOrForwardPrice(index2, _)))
    val avg3 = Quantity.average(rule.observationDays(index.calendars, period).map(env.fixingOrForwardPrice(index3, _)))

    assertQtyEquals(forwardPrice, avg1 * .5 - avg2 * .4 - avg3 * 1.2)
  }

  @Test(expectedExceptions = Array(classOf[InvalidFormulaIndexException]))
  def testVerify {
    val index = FormulaIndex("0.5 WTI vs 0.4 HO + 1.2 ?", Formula("0.5*MKT(7) - 0.4 *MKT(29) - 1.2 *MKT(1432)"), USD, BBL, None, None, None)
    index.verify
  }

}
