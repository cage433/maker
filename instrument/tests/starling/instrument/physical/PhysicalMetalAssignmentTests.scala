package starling.instrument.physical

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.market.LmeSingleIndices
import starling.daterange.{Month, Day}
import scala.collection.immutable.TreeMap
import starling.daterange.DateRange
import starling.quantity.{Percentage, UOM, Quantity}
import starling.market.Lead
import starling.market.Commodity
import starling.market.Copper
import starling.quantity.utils.QuantityTestUtils._
import starling.marketdata._
import starling.curves._

/**
 * Commenting out until everything settles down - AMc 29/9/11
 */
class PhysicalMetalAssignmentTests extends StarlingTest {
  val marketDay = Day(2011, 8, 10).endOfDay
  val env = Environment(
    new UnitTestingAtomicEnvironment(
      marketDay, 
      {
        case _: ForwardPriceKey => Quantity(97, USD/MT)
        case _: IndexFixingKey => Quantity(98, USD/MT)
        case DiscountRateKey(_, day, _) => new Quantity(math.exp(- 0.1 * day.endOfDay.timeSince(marketDay)))
      }
    ){
      override def referenceDataLookup = new ReferenceDataLookup.NullReferenceDataLookup() {
        override val countries = Map.empty[NeptuneCountryCode, NeptuneCountry].withDefaultValue(
          NeptuneCountry(NeptuneCountryCode("DummyCountryCode"), "DummyCountry", Some(Area(AreaCode.EUR, "Dummy"))))
      }
  }
  )
  

  @Test
  def testMixedCurrenciesAndVolumes{
    val contractPricingSpec = AveragePricingSpec(
      LmeSingleIndices.cuCashOffer,
      Month(2012, 1),
      Quantity(1.5, EUR/KG),
      EUR
    )
    val marketDay = Day(2011, 10, 17).endOfDay
    import Quantity._
    val fxRates = Map(
      EUR → 1.1(USD/EUR),
      GBP → 0.8(USD/GBP),
      CNY → 0.1(USD/CNY)
    )
    val env = Environment(
      new UnitTestingAtomicEnvironment(
        marketDay, 
        {
          case ForwardPriceKey(mkt, _, _) => Quantity(97, mkt.priceUOM)
          case IndexFixingKey(index, _) => Quantity(98, index.priceUOM)
          case DiscountRateKey(_, day, _) => new Quantity(math.exp(- 0.1 * day.endOfDay.timeSince(marketDay)))
          case USDFXRateKey(ccy) => fxRates(ccy)
          case _ : CountryBenchmarkAtomicKey => Quantity(115, GBP / G)
          case _ : AreaBenchmarkAtomicKey => Quantity(0, GBP / G)
          case _ : FreightParityAtomicKey => Quantity(5, CNY / LB)

        }
      ){
        override def referenceDataLookup = new ReferenceDataLookup.NullReferenceDataLookup(){
          override val countries = Map.empty[NeptuneCountryCode, NeptuneCountry].withDefaultValue(
            NeptuneCountry(NeptuneCountryCode("DummyCountryCode"), "DummyCountry", Some(Area(AreaCode.EUR, "Dummy"))))
//          override val areas = Map.empty[AreaCode, Area]
        }
      }
    )
    val pma = PhysicalMetalAssignment(
      "Assignment ID",
      Quantity(1000, KG),
      Copper,
      Day(2011, 11, 20),   // Contract delivery day
      contractPricingSpec,
      ContractualLocationCode("Somewhere in London"),
      IncotermCode("CIF"),
      Some(Day(2011, 12, 20)),   // benchmark delivery day
      Some(NeptuneCountryCode("Texas")),
      Some(IncotermCode("CIF")),
      true,                // is purchase
      "Inventory ID",
      Quantity(1.1, MT),   // Inventory Quantity
      GradeCode("Pretty Good")
    )
    val mtm = pma.mtm(env)
    val exp = pma.explanation(env)
    assertQtyEquals(mtm, exp, 1e-6)
    assertEquals(mtm.uom, contractPricingSpec.valuationCCY)
  }

}

