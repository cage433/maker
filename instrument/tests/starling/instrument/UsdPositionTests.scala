package starling.instrument

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.daterange.Day
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.curves._
import starling.utils.QuantityTestUtils._
import starling.market.Market

class UsdPositionTests extends StarlingTest {
  val market = Market.LME_COPPER
  val price: Quantity = Quantity(123, USD / MT)
  val usdRates = Map(
    GBP -> Quantity(1.8, USD / GBP),
    EUR -> Quantity(0.9, USD / EUR)
  )

  val env = Environment(
    new TestingAtomicEnvironment{
      def marketDay = Day(2009, 1, 1).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case _ : ForwardPriceKey => {
          price
        }
        case USDFXRateKey(ccy) => usdRates(ccy)

        case DiscountRateKey(ccy, _, _) => 1.0
      }
    })

  @Test
  def testUsdPricedForwardHasUsdPositionEqualToTheValueOfTheUnderlying{

    val forward = CommodityForward(market, Day(2010, 2, 1), Quantity(20, USD/MT), Quantity(100, MT))
    assertQtyEquals(forward.usdDeltaPosition(env), forward.volume * price, 1e-6)
  }

  @Test
  def testCrossCurrencyForwardHasUsdPositionThatTakesAccoountOfStrike{
    val forward = CommodityForward(market, Day(2010, 2, 1), Quantity(20, GBP/MT), Quantity(100, MT))
    assertQtyEquals(
      forward.usdDeltaPosition(env),
      forward.volume * (price - forward.strike * usdRates(GBP)),
      1e-6)
  }

  @Test
  def testFxForwardHasUSDPositionBasedOnTheNonUsdCurrency{
    val fwd = FXForward(Quantity(2.5, USD/GBP), Quantity(100, GBP), Day(2010, 1, 1))
    assertQtyEquals(
      fwd.asUtpPortfolio.usdDeltaPosition(env),
      fwd.volume * usdRates(GBP),
      1e-6)
  }

  @Test
  def testCrossCurrencyFXForwardHasUsdPositionFromBothLegs{
    val fwd = FXForward(Quantity(2.5, EUR/GBP), Quantity(100, GBP), Day(2010, 1, 1))
    assertQtyEquals(
      fwd.asUtpPortfolio.usdDeltaPosition(env),
      fwd.volume * usdRates(GBP) - fwd.volume * fwd.strike * usdRates(EUR),
      1e-6)

  }
}
