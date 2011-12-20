package starling.instrument.physical

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.daterange.{Month, Day}
import starling.quantity.Quantity
import starling.quantity.utils.QuantityTestUtils._
import starling.marketdata._
import starling.curves._
import Quantity._
import starling.market._
import starling.market.FuturesFrontPeriodIndex._

/**
 * Commenting out until everything settles down - AMc 29/9/11
 */
class PhysicalMetalAssignmentTests extends StarlingTest {

  val marketDay = Day(2011, 10, 17).endOfDay
  import Quantity._
  val fxRates = Map(
    EUR → 1.1(USD/EUR),
    GBP → 0.8(USD/GBP),
    CNY → 0.1(USD/CNY)
  )
  val env = UnitTestingEnvironment(
    marketDay,
    {
      case ForwardPriceKey(mkt, _, _) => Quantity(97, mkt.priceUOM)
      case IndexFixingKey(index, _) => Quantity(98, index.priceUOM)
      case DiscountRateKey(_, day, _) => new Quantity(math.exp(- 0.1 * day.endOfDay.timeSince(marketDay)))
      case USDFXRateKey(ccy) => fxRates(ccy)
      case _ : CountryBenchmarkAtomicKey => Quantity(115, GBP / G)
      case _ : AreaBenchmarkAtomicKey => Quantity(0, GBP / G)
      case _ : FreightParityAtomicKey => Quantity(5, CNY / LB)
      case _ : ShanghaiVATRateKey => new Quantity(0.17)

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
    assertQtyEquals(mtm, exp, 1e-6, "explanation quantity does not match the mtm quantity")
    assertEquals(mtm.uom, contractPricingSpec.valuationCCY)
  }


  /**
   * Check benchmark quantity handling using differing inventory quantities and assert MTM and explanations match and that the
   *   inventory quantity causes a change in mtm / exp quantity
   */
  @Test
  def testBenchmarkQuantities{

    val currentInventoryQty1 = Quantity(0.9, MT)   // Inventory Quantity

    val contractPricingSpec = AveragePricingSpec(
      LmeSingleIndices.cuCashOffer,
      Month(2012, 1),
      Quantity(1.5, EUR/KG),
      EUR
    )

    val pma1 = PhysicalMetalAssignment(
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
      currentInventoryQty1,
      GradeCode("Pretty Good")
    )

    val mtm1 = pma1.mtm(env)
    val exp1 = pma1.explanation(env)
    assertQtyEquals(mtm1, exp1, 1e-6)
    assertEquals(mtm1.uom, contractPricingSpec.valuationCCY)


    val actualQuantity = Quantity(1000, KG)
    val currentInventoryQty2 = Quantity(700, KG)   // Inventory Quantity

    val pma2 = PhysicalMetalAssignment(
      "Assignment ID",
      actualQuantity,
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
      currentInventoryQty2,
      GradeCode("Pretty Good")
    )

    val mtm2 = pma2.mtm(env)
    val exp2 = pma2.explanation(env)
    assertQtyEquals(mtm2, exp2, 1e-6)
    assertEquals(mtm2.uom, contractPricingSpec.valuationCCY)

    // assert that changes inventory qty caused a difference in explain
    assertQtyNotEquals(exp1, exp2, 1e-6)

    // and that the weight/loss gain reflects the difference of quantity vs. (current) inventory quantity
    assertQtyEquals(pma2.weightGain, currentInventoryQty2 - actualQuantity, 1e-6)
  }

  @Test
  def testValuationSnapshots{

    val specs = List(
      FixedPricingSpec(Market.LME_COPPER, Day(2013, 1, 1), List((0.3, Quantity(20, USD/MT)), (0.7, Quantity(25, USD/MT))), Quantity(1.5, USD/MT), EUR),
      FixedPricingSpec(Market.SHANGHAI_COPPER, Day(2013, 1, 1), List((0.3, Quantity(20, CNY/MT)), (0.7, Quantity(25, CNY/MT))), Quantity(1.5, CNY/MT), EUR),
      UnknownPricingSpecification(LmeCashSettlementIndex(Market.LME_NICKEL, Level.Ask), Month(2012, 4), List(UnknownPricingFixation(0.2, 123.0 (USD/MT))), Day(2012, 4, 30), 1.2 (USD/MT), EUR),
      UnknownPricingSpecification(FuturesFrontPeriodIndex(Market.SHANGHAI_COPPER), Month(2012, 4), List(UnknownPricingFixation(0.2, 123.0 (CNY/MT))), Day(2012, 4, 30), 1.2 (USD/MT), EUR),
      AveragePricingSpec(LmeThreeMonthIndex(Market.LME_ALUMINIUM, Level.Bid), Month(2012, 5), 1.5 (USD/MT), GBP),
      AveragePricingSpec(FuturesFrontPeriodIndex(Market.SHANGHAI_ALUMINUIUM), Month(2012, 5), 1.5 (USD/MT), GBP)
    )

    val commodities = List(Copper, Copper, Nickel, Copper, Aluminium, Aluminium)
    val assignments = specs.zip(commodities).zipWithIndex.map{
      case ((spec, commodity), i) =>
        PhysicalMetalAssignment(
          i.toString,
          (110.0 + i) (MT),
          commodity,
          spec.expiryDay,
          spec,
          ContractualLocationCode("lc"),
          IncotermCode("CIF"),
          Some(spec.expiryDay),
          Some(NeptuneCountryCode("Texas")),
          Some(IncotermCode("CIF")),
          true,
          "Inv id" + i,
          (110.0 + i + 1) (MT),
          GradeCode("Ok-ish")
        )
    }

    // some values changed as a result of VAT by currency rather than exchange, previous expected numbers in comments below
    val expectedMtms : List[Quantity] = List(
      8191103481.95(EUR),
      8230995765.91(EUR),
      8917616273.77(EUR),
      9036057110.48(EUR),
      12359657400.15(GBP),
      12535633384.33(GBP)
    )
    assignments.foreach{ass => println(ass.mtm(env))}
    assignments.zip(expectedMtms).foreach {
      case (ass, expMtm) =>
        assertQtyEquals(ass.mtm(env), expMtm, 1e-2, "assignmentMtm does not match the expected value")
    }
  }
}

object PhysicalMetalAssignmentTests extends App{
  println("Hello")
  new PhysicalMetalAssignmentTests().testValuationSnapshots
}
