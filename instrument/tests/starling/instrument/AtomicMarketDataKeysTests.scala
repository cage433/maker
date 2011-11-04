package starling.instrument

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.curves._
import starling.quantity.UOM._
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.{Percentage, Quantity}
import starling.daterange.{Month, Day, DayAndTime}
import starling.models.{Call, European}
import starling.market._
import starling.marketdata.ReferenceDataLookup
import org.scalatest.testng.TestNGSuite

class AtomicMarketDataKeysTests extends TestMarketTest with TestNGSuite {
  val market = Market.NYMEX_WTI
  val index = Index.WTI10

  val env = Environment(
    new TestingAtomicEnvironment() {
      def applyOrMatchError(key: AtomicDatumKey): Any = key match {
        case DiscountRateKey(ccy, day, _) => new Quantity(math.exp(-.05 * day.daysSinceInYears(marketDay.day)))
        case ForwardPriceKey(market, _, _) => Quantity(80, market.priceUOM)
        case USDFXRateKey(ccy) => Quantity(1.0, USD / ccy)
        case _: OilAtmVolAtomicDatumKey => Percentage(10)
        case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
      }
      def marketDay: DayAndTime = Day(2009, 10, 1).endOfDay
    })

  @Test
  def testFuture {
    val delivery = Month(2010, 1)
    val future = new Future(market, delivery, Quantity(20, USD / BBL), Quantity(1, BBL))
    assertEquals(future.atomicMarketDataKeys(env), Set(ForwardPriceKey(market, delivery)))
  }

  @Test
  def testFuturesOption {
    val delivery = Month(2010, 1)
    val oilMarket = market
    val option = new FuturesOption(market, market.optionExpiry(delivery), delivery, Quantity(20, USD / BBL), Quantity(1, BBL), Call, European)
    assertEquals(option.atomicMarketDataKeys(env), Set(ForwardPriceKey(market, delivery), OilVolSkewAtomicDatumKey(oilMarket, delivery),
      OilAtmVolAtomicDatumKey(oilMarket, None, delivery, false),
      DiscountRateKey(USD,market.optionExpiry(delivery))))
  }

  @Test
  def testAsianOption {
    val delivery = Month(2010, 1)
    val oilMarket = market
    val option = new SingleAsianOption(index, delivery, Quantity(20, USD / BBL), Quantity(1, BBL), Call)

    //val oilVolAtmKeys = Set(OilAtmVolAtomicDatumKey(oilMarket, None, delivery + 1), OilAtmVolAtomicDatumKey(oilMarket, None, delivery + 2))
    val oilVolAtmKeys = index.observationDays(delivery).map{d => OilAtmVolAtomicDatumKey(oilMarket, Some(d), index.observedOptionPeriod(d))}
    val keys : Set[AtomicDatumKey] = Set(ForwardPriceKey(market, delivery + 1), ForwardPriceKey(market, delivery + 2),
      OilVolSkewAtomicDatumKey(oilMarket, delivery + 1), OilVolSkewAtomicDatumKey(oilMarket, delivery + 2),
      DiscountRateKey(USD, option.settlementDate), 
      DiscountRateKey(USD, market.optionExpiry(delivery + 1)), DiscountRateKey(USD, market.optionExpiry(delivery + 2))
      ) ++ oilVolAtmKeys 


    assertEquals(option.atomicMarketDataKeys(env), keys)
  }

  @Test
  def testSwap {
    val delivery = Month(2010, 1)
    val swap = new SinglePeriodSwap(index, Quantity(20, USD / BBL), Quantity(1, BBL), delivery, false)
    val settlement = CommoditySwap.swapSettlementDate(delivery.lastDay)
    assertEquals(swap.atomicMarketDataKeys(env), Set(ForwardPriceKey(market, delivery + 1), ForwardPriceKey(market, delivery + 2), DiscountRateKey(USD, settlement)))
  }

}
