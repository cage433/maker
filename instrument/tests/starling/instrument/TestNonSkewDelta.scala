package starling.instrument

import org.testng.annotations.Test
import starling.quantity.UOM._
import starling.curves._
import starling.quantity.{Percentage, Quantity}
import starling.daterange.{DateRange, Month, DayAndTime, Day}
import starling.calendar.{HolidayTablesFactory, BusinessCalendars, NullHolidays}
import starling.market._
import starling.quantity.utils.QuantityTestUtils._
import org.testng.Assert

class TestNonSkewDelta extends TestMarketTest{
  @Test
  def testVolatilityDoesntChange{
    val env = Environment{
      new TestingAtomicEnvironment{
        def marketDay = Day(2010, 1, 1).startOfDay
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => Quantity(100, USD/BBL)
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.5)
          case _ : OilVolSkewAtomicDatumKey => Map(0.25 -> Percentage(-0.02), 0.5 -> Percentage(0.01), 0.75 -> Percentage(0.04))
          case _ : DiscountRateKey => new Quantity(1.0)
        }
      }
    }.undiscounted

    val inst = new Instrument(){
      
      def pivotUTPType = throw new Exception("Unimplemented")

      def explanation(env: Environment) = throw new Exception("Unimplemented")

      def atomicMarketDataKeys = Set[AtomicDatumKey]()

      def isLive(dayAndTime: DayAndTime) = true

      def valuationCCY = USD

      def assets(env: Environment) = {
        val F = env.forwardPrice(Market.ICE_BRENT, Month(2011, 1))
        val sigma = env.impliedVol(Market.ICE_BRENT, Month(2011, 1), Day(2010, 12, 31), Quantity(110, USD/BBL))
        val mtm = Quantity(F.value + 100 * sigma.decimalValue, USD)
        Assets(Asset(true, "fred", Day(2010, 12, 31), Quantity(1, BBL), mtm))
      }

    }

    val differentiable: PriceDifferentiable = PriceDifferentiable(Market.ICE_BRENT, DateRange(env.marketDay.day, env.marketDay.day + 600))
    val skewDelta = inst.firstOrderDerivative(env, differentiable, USD)
    Assert.assertTrue(skewDelta.value < 0.9, "SkewDelta: " + skewDelta)

    val nonSkewEnv = env.setShiftsCanBeIgnored(true)

    var nonSkewDelta = inst.firstOrderDerivative(nonSkewEnv, differentiable, USD)
    assertQtyEquals(nonSkewDelta, Quantity(1.0, BBL), 1e-6)

    val muchShiftedNonSkewEnv = nonSkewEnv.shiftPrice(Market.ICE_BRENT, Month(2011, 1), Quantity(1, USD/BBL)).forwardState(Day(2010, 1, 3).endOfDay).shiftFwdFwdRate(USD, Month(2010, 12), new Quantity(1e-4))
    nonSkewDelta = inst.firstOrderDerivative(muchShiftedNonSkewEnv, differentiable, USD)
    assertQtyEquals(nonSkewDelta, Quantity(1.0, BBL), 1e-6)
  }

}
