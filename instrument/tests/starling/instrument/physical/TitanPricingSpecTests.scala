package starling.instrument.physical

import org.scalatest.FunSuite
import starling.curves.{ForwardPriceKey, IndexFixingKey, ShanghaiVATRateKey, UnitTestingEnvironment}
import starling.daterange.{Day, Month}
import starling.market.{FuturesFrontPeriodIndex, Market, TestMarketTest}
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.quantity.utils.QuantityTestUtils._
import starling.curves.USDFXRateKey
import starling.curves.DiscountRateKey


class TitanPricingSpecTests extends FunSuite with TestMarketTest{
  test("Prices inclusive/exclusive of VAT are sensible"){
    val specs = List(
      AveragePricingSpec(
        FuturesFrontPeriodIndex(Market.SHANGHAI_ZINC),
        Month(2011, 12),
        Quantity(10, USD/MT),
        GBP
      ),
      UnknownPricingSpecification(
        FuturesFrontPeriodIndex(Market.SHANGHAI_COPPER),
        Month(2012, 1),
        List(UnknownPricingFixation(0.4, Quantity(100, USD/MT))),
        Day(2012, 1, 31),
        Quantity(15, GBP/MT),
        EUR
      )
    )

    val fxRates = Map(
      EUR -> Quantity(1.5, USD/EUR),
      GBP -> Quantity(1.2, USD/GBP),
      CNY -> Quantity(6.2, USD/CNY)
    )
    val zeroRates = Map(
      EUR → 0.1,
      GBP → 0.2,
      CNY → 0.2,
      USD → 0.25
    )

    val marketDay = Day(2011, 12, 1).endOfDay
    val env = UnitTestingEnvironment(
      marketDay,
      {
        case ForwardPriceKey(mkt, _, _) => Quantity(100, mkt.priceUOM)
        case IndexFixingKey(index, _) => Quantity(99, index.priceUOM)
        case ShanghaiVATRateKey() => Quantity(17, PERCENT)
        case USDFXRateKey(ccy) => fxRates(ccy)
        case DiscountRateKey(ccy, day, _) => new Quantity(math.exp(- zeroRates(ccy) * day.endOfDay.timeSince(marketDay)))
      }
    )
    
    specs.foreach{spec =>
      val priceExclVAT = spec.priceExcludingVAT(env)
      val priceInclVAT = spec.priceIncludingVAT(env).get
      val vatRate = env.shanghaiVATRate
      assertQtyEquals(priceExclVAT * (vatRate + 1.0), priceInclVAT, 1e-6)
      assert(priceInclVAT.numeratorUOM === spec.valuationCCY)
    }
  }
}


