package starling.varcalculator

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.quantity.UOM._
import org.testng.Assert._
import starling.instrument.FuturesOption
import starling.market.Market._
import starling.daterange.Day._
import starling.daterange.Month
import starling.models.{European, Call}
import starling.curves._
import starling.quantity.{Percentage, Quantity}
import starling.market.TestExpiryRules

class VolatilityRiskFactorTests extends TestExpiryRules {

  @Test
  def test {
    val delivery = Month(2013, 1)
    val option = new FuturesOption(NYMEX_WTI, NYMEX_WTI.optionExpiry(delivery), delivery, Quantity(10, USD / BBL), Quantity(1, BBL), Call, European)

    val env = Environment(
      new TestingAtomicEnvironment(){
        val marketDay = (10 Jan 2010).endOfDay
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _: ForwardPriceKey => Quantity(12, USD / MT)
          case _: OilAtmVolAtomicDatumKey => Percentage(.20)
          case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
        }
      }
      ).undiscounted

    val riskFactors = option.riskFactors(env, USD)
    assertTrue(riskFactors.contains(VolatilityRiskFactor(NYMEX_WTI, Month(2013, 1))), "Missing vol risk factor: " + riskFactors)
  }
}
