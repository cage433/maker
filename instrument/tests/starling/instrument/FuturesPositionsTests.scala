package starling.instrument

import starling.daterange.{DayAndTime, Day}
import starling.curves._
import starling.quantity.Quantity
import starling.quantity.UOM._
import org.testng.annotations.Test
import starling.varcalculator.ForwardPriceRiskFactor
import org.testng.Assert
import starling.quantity.utils.QuantityTestUtils._
import starling.market.Market._
import starling.market.{TestMarketTest, CommodityMarket}

class FuturesPositionsTests extends TestMarketTest {
  private val prices:Map[CommodityMarket, Double] = Map(LME_LEAD -> 123.0, LME_ZINC -> 99.0, NYMEX_WTI -> 108.0,
    NYMEX_GASOLINE -> 88.0)

  def env(zeroRate : Double) = Environment(
      new TestingAtomicEnvironment(){
        def applyOrMatchError(key : AtomicDatumKey) : Any = key match {
          case DiscountRateKey(ccy, day, _) => new Quantity(math.exp(- zeroRate * day.daysSinceInYears(marketDay.day)))
          case ForwardPriceKey(market, _, _) => Quantity(prices(market), market.priceUOM)
          case USDFXRateKey(ccy) => Quantity(1.0, USD/ccy)
        }
        def marketDay : DayAndTime = Day(2009, 10, 1).endOfDay

      })

  @Test
  def testFuturesPositionsMatchExpectedLots{
    List(LME_LEAD, LME_ZINC).foreach{
      mkt =>
        val volume = Quantity(mkt.commodity.representativeMarket.lotSize.get * 9, mkt.uom)
        val future = new Future(mkt, mkt.frontPeriod(Day(2010, 10, 1)), Quantity(103, mkt.priceUOM), volume)
        val position = future.commodityFuturesPosition(env(0.0), ForwardPriceRiskFactor(mkt, 0, 1000))
        assertQtyEquals(position, Quantity(9, mkt.commodity.standardFuturesUOM), 1e-6)
    }
  }

  @Test
  def wtestFXInstrumentHasNoFuturesPosition{
    val fx = new FXForward(Quantity(1.5, USD/GBP), Quantity(100, GBP), Day(2010, 10, 1))
    val utpPortfolio = fx.asUtpPortfolio
    for (rf <- utpPortfolio.riskFactors(env(0), USD)){
      val riskFactorPosition = utpPortfolio.riskFactorPosition(env(0.0), rf, USD)
      val position = utpPortfolio.commodityFuturesPosition(rf, riskFactorPosition)
      Assert.assertEquals(position, Quantity.NULL)
    }
  }
}
