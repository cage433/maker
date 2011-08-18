package starling.instrument

import starling.utils.StarlingTest
import org.testng.Assert._
import org.testng.annotations.Test
import starling.quantity.RichQuantity._
import starling.quantity.UOM._
import starling.quantity.Percentage
import starling.daterange.{Day, Year}
import starling.daterange.Day._
import starling.curves.{USDFXRateKey, AtomicDatumKey, Environment, TestingAtomicEnvironment}
import starling.instrument.CashInstrumentType._
import starling.market.{Index, TestMarketTest, Market}

class BrokerCostTests extends TestMarketTest {
  val env = Environment(
    new TestingAtomicEnvironment() {
      def marketDay = (1 Jan 2010).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case USDFXRateKey(USD) => 1.0
      }
    }
    ).undiscounted

  @Test
  def testSign {
    val cit = BrokerPayment
    List(new SinglePayment(Day.today, (-1)(BBL)), new SinglePayment(Day.today, 1(BBL))).map(payment => {

      assertTrue(payment.costs(cit, CommissionPercent(Percentage(.1), 10(USD / DAY), USD)).forall(_.volume.isNegativeOrZero))
      assertTrue(payment.costs(cit, CommissionPercent(Percentage(.1), -(10)(USD / DAY), USD)).forall(_.volume.isNegativeOrZero))

      assertTrue(payment.costs(cit, CommissionLumpSum(10(USD))).forall(_.volume.isNegativeOrZero))
      assertTrue(payment.costs(cit, CommissionLumpSum(-(10)(USD))).forall(_.volume.isNegativeOrZero))

      assertTrue(payment.costs(cit, CommissionLumpSumAmountMultiple(10(USD))).forall(_.volume.isNegativeOrZero))
      assertTrue(payment.costs(cit, CommissionLumpSumAmountMultiple(-(10)(USD))).forall(_.volume.isNegativeOrZero))
    })

  }

  @Test
  def testAtMonthEnd {
    val period = Year(2011)
    val mkt = Index.CAPSIZE_TC_AVG
    val vol = 15(DAY)
    val volume = new FreightVolume(mkt.uom, vol, period)
    val atMonthEnd = new AtMonthEndFreight(period, volume)
    val percentage = new Percentage(0.00125)
    val strike = 14000(USD / DAY)
    val commission = new CommissionPercent(percentage, strike, USD)

    val brokerCosts = new BrokerCosts("Some Broker", atMonthEnd, commission)
    val costs = Set() ++ brokerCosts.costs

    assertEquals(costs, Set() ++ volume.months.map(m => new CashInstrument(BrokerPayment, -(262.5 (USD)), m.lastDay)))
    assertEquals(costs, Set() ++ volume.months.map(m => new CashInstrument(BrokerPayment, -commission.amount(vol), m.lastDay)))

    val expected = -(strike * vol * 12.0 * percentage).value (USD)
    assertEquals(brokerCosts.mtm(env, USD), expected)
  }

  @Test
  def testAtMonthEndPercentVolume {
    val period = Year(2011)
    val mkt = Index.CAPSIZE_TC_AVG
    val vol = 100(PERCENT)
    val strike = 14000(USD / DAY)
    val volume = new FreightVolume(mkt.uom, vol, period)
    val atMonthEnd = new AtMonthEndFreight(period, volume)
    val percentage = new Percentage(0.005)
    val commission = new CommissionPercent(percentage, strike, USD)

    val brokerCosts = new BrokerCosts("Some Broker", atMonthEnd, commission)
    val costs = Set() ++ brokerCosts.costs

    assertEquals(costs, Set() ++ volume.months.map(m => new CashInstrument(BrokerPayment, -commission.amount(m.days.size(DAY)), m.lastDay)))

    val expected = -(strike * 365.0 * percentage).value (USD)
    assertEquals(brokerCosts.mtm(env, USD), expected)
  }

  @Test
  def testSinglePayment {
    val period = Year(2011)
    val mkt = Index.CAPSIZE_TC_AVG
    val vol = 100(DAY)
    val volume = new FreightVolume(mkt.uom, vol, period)
    val singlePayment = new SingleFreightPayment(Day.today, volume)
    val commission = new CommissionLumpSum(30(USD))

    val costs = new BrokerCosts("Some Broker", singlePayment, commission).costs

    assertTrue(costs.size == 1)
    assertEquals(costs.head, new CashInstrument(BrokerPayment, -(30(USD)), Day.today))
  }

}
