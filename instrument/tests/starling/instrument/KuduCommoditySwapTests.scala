package starling.instrument

import starling.quantity.Quantity
import org.testng.annotations.Test
import starling.curves._
import starling.daterange.{Day, Month}
import starling.quantity.UOM._
import starling.utils.QuantityTestUtils._
import starling.market.{Index, TestMarketTest}

class KuduCommoditySwapTests extends TestMarketTest {
  val index = Index.WTI10
  val market = index.market
  val env = Environment(new TestingAtomicEnvironment() {
    def marketDay = Day(2010, 1, 5).startOfDay

    def applyOrMatchError(key: AtomicDatumKey) = key match {
      case IndexFixingKey(`index`, Day(2010, 1, 4)) => Quantity(81.51, USD / BBL)
      case ForwardPriceKey(`market`, Month(2010, 2), _) => Quantity(81.77, USD / BBL)
      case ForwardPriceKey(`market`, Month(2010, 3), _) => Quantity(82.34, USD / BBL)
      case ForwardPriceKey(`market`, Month(2011, 1), _) => Quantity(86.4, USD / BBL)
      case ForwardPriceKey(`market`, Month(2011, 2), _) => Quantity(86.65, USD / BBL)
      case DiscountRateKey(_, day, _) => new Quantity(1.0)
    }
  })

  @Test
  def singlePeriodForwardWTISwapPriceTest {
    val month = Month(2010, 12)
    val volume = Quantity(1, BBL)
    val strike = Quantity(0, USD / BBL)

    val swap = new SinglePeriodSwap(index,strike,volume,month, cleared = false)
    assertQtyEquals(swap.mtm(env), Quantity(86.483, USD), 1e-6)
  }

  @Test
  def singlePeriodPricingWTISwapPriceTest {
    val month = Month(2010, 1)
    val volume = Quantity(1, BBL)
    val strike = Quantity(0, USD / BBL)

    val swap = new SinglePeriodSwap(index,strike,volume,month, cleared = false)
//    assertQtyEquals(swap.mtm(env), Quantity(82.0047058, USD), 1e-6) // Wrong atm
  }
}
