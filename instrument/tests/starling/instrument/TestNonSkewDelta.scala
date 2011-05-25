package starling.instrument

import org.testng.annotations.Test
import starling.quantity.UOM._
import starling.curves._
import starling.quantity.{Percentage, Quantity}
import starling.daterange.{DateRange, Month, DayAndTime, Day}
import starling.calendar.{HolidayTablesFactory, BusinessCalendars, NullHolidays}
import starling.varcalculator.ForwardPriceRiskFactor
import starling.market._
import starling.utils.QuantityTestUtils._
import org.testng.Assert

class TestNonSkewDelta extends TestExpiryRules{
  @Test
  def testVolatilityDoesntChange{
    val env = Environment{
      new TestingAtomicEnvironment{
        def marketDay = Day(2010, 1, 1).startOfDay
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _ : ForwardPriceKey => Quantity(100, USD/BBL)
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.5)
          case _ : OilVolSkewAtomicDatumKey => Map(0.25 -> Percentage(-0.02), 0.5 -> Percentage(0.01), 0.75 -> Percentage(0.04))
          case _ : DiscountRateKey => 1.0
        }
      }
    }.undiscounted

    val inst = new Instrument(){
      
      def pivotUTPType = throw new Exception("Unimplemented")

      def atomicMarketDataKeys = Set[AtomicDatumKey]()

      def isLive(dayAndTime: DayAndTime) = true

      def valuationCCY = USD

      def assets(env: Environment) = {
        val F = env.forwardPrice(Market.ICE_BRENT, Month(2011, 1))
        val sigma = env.impliedVol(Market.ICE_BRENT, Month(2011, 1), Day(2010, 12, 31), Quantity(110, USD/BBL))
        val mtm = Quantity(F.value + 100 * sigma.decimalValue, USD)
        Assets(Asset(true, false, "fred", Day(2010, 12, 31), Quantity(1, BBL), mtm))
      }

    }

    val skewDelta = inst.riskFactorDerivative(env, ForwardPriceRiskFactor(Market.ICE_BRENT, 0, 20), USD)
    Assert.assertTrue(skewDelta.value < 0.9, "SkewDelta: " + skewDelta)

    val nonSkewEnv = env.setShiftsCanBeIgnored(true)

    var nonSkewDelta = inst.riskFactorDerivative(nonSkewEnv, ForwardPriceRiskFactor(Market.ICE_BRENT, 0, 20), USD)
    assertQtyEquals(nonSkewDelta, Quantity(1.0, BBL), 1e-6)

    val muchShiftedNonSkewEnv = nonSkewEnv.shiftPrice(Market.ICE_BRENT, Month(2011, 1), Quantity(1, USD/BBL)).forwardState(Day(2010, 1, 3).endOfDay).shiftFwdFwdRate(USD, Month(2010, 12), new Quantity(1e-4))
    nonSkewDelta = inst.riskFactorDerivative(muchShiftedNonSkewEnv, ForwardPriceRiskFactor(Market.ICE_BRENT, 0, 20), USD)
    assertQtyEquals(nonSkewDelta, Quantity(1.0, BBL), 1e-6)
  }

}
