package starling.instrument

import starling.utils.StarlingTest
import org.testng.Assert._
import org.testng.annotations.Test
import starling.quantity.RichQuantity._
import starling.quantity.UOM._
import starling.quantity.Percentage
import starling.market.{TestExpiryRules, Market}
import starling.daterange.{Day, Year}
import starling.instrument.CashInstrumentType._

class ClearingHouseCostsTests extends TestExpiryRules {

  @Test
  def testSinglePayment {
    val period = Year(2011)
    val mkt = Market.BALTIC_CAPESIZE
    val vol = 100(DAY)
    val volume = new FreightVolume(mkt.uom, vol, period)
    val singlePayment = new SingleFreightPayment(Day.today, volume)
    // fees for clearing houses are per day
    val fee = .30 (USD) / 1.0 (mkt.uom)
    val commission = new CommissionLumpSumAmountMultiple(fee)

    val costs = new ClearingHouseCosts("Some CH", singlePayment, commission).costs

    assertTrue(costs.size == 1)
    assertEquals(costs.head, new CashInstrument(ClearingHousePayment, -fee * volume.totalAmount, Day.today))
  }

}
