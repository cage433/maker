package starling.instrument.physical

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.market.LmeSingleIndices
import starling.daterange.{Month, Day}
import starling.curves.Environment
import starling.curves.UnitTestingAtomicEnvironment
import starling.curves.ForwardPriceKey
import starling.curves.DiscountRateKey
import starling.curves.IndexFixingKey
import scala.collection.immutable.TreeMap
import starling.daterange.DateRange
import starling.quantity.{Percentage, UOM, Quantity}
import starling.market.Lead
import starling.market.Commodity
import starling.market.Copper
import starling.quantity.utils.QuantityTestUtils._
import starling.curves.USDFXRateKey
import starling.curves.CountryBenchmarkAtomicKey
import starling.curves.FreightParityAtomicKey
import starling.gui.api.{GradeCode, ContractualLocationCode, IncotermCode, NeptuneCountryCode}

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
    )
  )


  //@Test
  def testAverageExplanation{
    val monthSpec = AveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 8), Quantity(1.5, USD/MT))

    val assignment = PhysicalMetalAssignment(
      "1234", Quantity(100, MT), Lead, Day(2011, 9, 1), monthSpec, ContractualLocationCode("France"), IncotermCode("CIF"),
      benchmarkDeliveryDay = None, benchmarkCountryCode = Some(NeptuneCountryCode("Italy")), benchmarkIncoTermCode = Some(IncotermCode("CIF")),
      isPurchase = true, inventoryID = "inventory id", inventoryQuantity = Quantity(90, MT),
      grade = GradeCode("grade")
    )

    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "-((Contract Price * Volume) * Discount)")
    assertEquals(explanation.format(1), "-(((Average(LME Copper cash Bid.AUGUST 2011) + Premium) * 100.00 MT) * 0.99)")
    val ex = "-(((Average(01Aug2011, 02Aug2011, 03Aug2011, 04Aug2011, 05Aug2011, 08Aug2011, 09Aug2011, 10Aug2011, 11Aug2011, 12Aug2011, 15Aug2011, 16Aug2011, 17Aug2011, 18Aug2011, 19Aug2011, 22Aug2011, 23Aug2011, 24Aug2011, 25Aug2011, 26Aug2011, 29Aug2011, 30Aug2011, 31Aug2011) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(2), ex)
    val ex2 = "-(((Average(LME Copper.03Aug2011 Fixed, LME Copper.04Aug2011 Fixed, LME Copper.05Aug2011 Fixed, LME Copper.08Aug2011 Fixed, LME Copper.09Aug2011 Fixed, LME Copper.10Aug2011 Fixed, LME Copper.11Aug2011 Fixed, LME Copper.12Aug2011 Fixed, LME Copper.15Aug2011, LME Copper.16Aug2011, LME Copper.17Aug2011, LME Copper.18Aug2011, LME Copper.19Aug2011, LME Copper.22Aug2011, LME Copper.23Aug2011, LME Copper.24Aug2011, LME Copper.25Aug2011, LME Copper.26Aug2011, LME Copper.29Aug2011, LME Copper.30Aug2011, LME Copper.31Aug2011, LME Copper.01Sep2011, LME Copper.02Sep2011) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(3), ex2)
    val lastExplanation = "-(((Average(98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(4), lastExplanation)
    assertEquals(explanation.format(5), lastExplanation)
  }


  //@Test 
  def testWeightedAverageExplanation{
    val monthSpec = AveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 8), Quantity(1.5, USD/MT))
    val monthSpec2 = AveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 9), Quantity(0.5, USD/MT))
    val dayFractions = new TreeMap[Day, Double]() ++ Map(
      Day(2011, 8, 2) -> 0.1, 
      Day(2011, 8, 3) -> 0.1, 
      Day(2011, 8, 4) -> 0.2, 
      Day(2011, 8, 10) -> 0.1, 
      Day(2011, 8, 11) -> 0.1, 
      Day(2011, 8, 16) -> 0.2, 
      Day(2011, 8, 17) -> 0.1, 
      Day(2011, 8, 18) -> 0.1
    )
    val weightedAverageSpec = WeightedPricingSpec(List((0.4, monthSpec), (0.6, monthSpec2)))

    val assignment = PhysicalMetalAssignment(
      "1234", Quantity(100, MT), Lead, Day(2011, 9, 1), weightedAverageSpec, ContractualLocationCode("France"),IncotermCode("CIF"),
      benchmarkDeliveryDay = None, benchmarkCountryCode = Some(NeptuneCountryCode("Italy")), benchmarkIncoTermCode = Some(IncotermCode("CIF")),
      isPurchase = true, inventoryID = "inventory id", inventoryQuantity = Quantity(90, MT),
      grade = GradeCode("grade")
    )

    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    //val ex = "((-Sum(((Average(CU Cash Bid.AUGUST 2011) + Premium) * 0.4), ((Sum((02Aug2011 * 0.1), (03Aug2011 * 0.1), (04Aug2011 * 0.2), (10Aug2011 * 0.1), (11Aug2011 * 0.1), (16Aug2011 * 0.2), (17Aug2011 * 0.1), (18Aug2011 * 0.1)) + Premium) * 0.6)) * 100.00 MT) * USD.02Sep2011)"
    val ex = "((-Sum(((Average(LME Copper cash Bid.AUGUST 2011) + Premium) * 0.4), ((Average(LME Copper cash Bid.SEPTEMBER 2011) + Premium) * 0.6)) * 100.00 MT) * USD.04Oct2011)"
    assertEquals(explanation.format(1), ex)
    //val ex2 = "((-Sum(((Average(01Aug2011, 02Aug2011, 03Aug2011, 04Aug2011, 05Aug2011, 08Aug2011, 09Aug2011, 10Aug2011, 11Aug2011, 12Aug2011, 15Aug2011, 16Aug2011, 17Aug2011, 18Aug2011, 19Aug2011, 22Aug2011, 23Aug2011, 24Aug2011, 25Aug2011, 26Aug2011, 29Aug2011, 30Aug2011, 31Aug2011) + 1.50 USD/MT) * 0.40), ((Sum((LME Copper.04Aug2011 Fixed * 0.10), (LME Copper.05Aug2011 Fixed * 0.10), (LME Copper.08Aug2011 Fixed * 0.20), (LME Copper.12Aug2011 Fixed * 0.10), (LME Copper.15Aug2011 * 0.10), (LME Copper.18Aug2011 * 0.20), (LME Copper.19Aug2011 * 0.10), (LME Copper.22Aug2011 * 0.10)) + 1.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    val ex2 = "((-Sum(((Average(01Aug2011, 02Aug2011, 03Aug2011, 04Aug2011, 05Aug2011, 08Aug2011, 09Aug2011, 10Aug2011, 11Aug2011, 12Aug2011, 15Aug2011, 16Aug2011, 17Aug2011, 18Aug2011, 19Aug2011, 22Aug2011, 23Aug2011, 24Aug2011, 25Aug2011, 26Aug2011, 29Aug2011, 30Aug2011, 31Aug2011) + 1.50 USD/MT) * 0.40), ((Average(01Sep2011, 02Sep2011, 05Sep2011, 06Sep2011, 07Sep2011, 08Sep2011, 09Sep2011, 12Sep2011, 13Sep2011, 14Sep2011, 15Sep2011, 16Sep2011, 19Sep2011, 20Sep2011, 21Sep2011, 22Sep2011, 23Sep2011, 26Sep2011, 27Sep2011, 28Sep2011, 29Sep2011, 30Sep2011) + 0.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(2), ex2)
    //val ex3 = "((-Sum(((Average(LME Copper.03Aug2011 Fixed, LME Copper.04Aug2011 Fixed, LME Copper.05Aug2011 Fixed, LME Copper.08Aug2011 Fixed, LME Copper.09Aug2011 Fixed, LME Copper.10Aug2011 Fixed, LME Copper.11Aug2011 Fixed, LME Copper.12Aug2011 Fixed, LME Copper.15Aug2011, LME Copper.16Aug2011, LME Copper.17Aug2011, LME Copper.18Aug2011, LME Copper.19Aug2011, LME Copper.22Aug2011, LME Copper.23Aug2011, LME Copper.24Aug2011, LME Copper.25Aug2011, LME Copper.26Aug2011, LME Copper.29Aug2011, LME Copper.30Aug2011, LME Copper.31Aug2011, LME Copper.01Sep2011, LME Copper.02Sep2011) + 1.50 USD/MT) * 0.40), ((Sum((98.00 USD/MT * 0.10), (98.00 USD/MT * 0.10), (98.00 USD/MT * 0.20), (98.00 USD/MT * 0.10), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.20), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.10)) + 1.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    val ex3 = "((-Sum(((Average(LME Copper.03Aug2011 Fixed, LME Copper.04Aug2011 Fixed, LME Copper.05Aug2011 Fixed, LME Copper.08Aug2011 Fixed, LME Copper.09Aug2011 Fixed, LME Copper.10Aug2011 Fixed, LME Copper.11Aug2011 Fixed, LME Copper.12Aug2011 Fixed, LME Copper.15Aug2011, LME Copper.16Aug2011, LME Copper.17Aug2011, LME Copper.18Aug2011, LME Copper.19Aug2011, LME Copper.22Aug2011, LME Copper.23Aug2011, LME Copper.24Aug2011, LME Copper.25Aug2011, LME Copper.26Aug2011, LME Copper.29Aug2011, LME Copper.30Aug2011, LME Copper.31Aug2011, LME Copper.01Sep2011, LME Copper.02Sep2011) + 1.50 USD/MT) * 0.40), ((Average(LME Copper.05Sep2011, LME Copper.06Sep2011, LME Copper.07Sep2011, LME Copper.08Sep2011, LME Copper.09Sep2011, LME Copper.12Sep2011, LME Copper.13Sep2011, LME Copper.14Sep2011, LME Copper.15Sep2011, LME Copper.16Sep2011, LME Copper.19Sep2011, LME Copper.20Sep2011, LME Copper.21Sep2011, LME Copper.22Sep2011, LME Copper.23Sep2011, LME Copper.26Sep2011, LME Copper.27Sep2011, LME Copper.28Sep2011, LME Copper.29Sep2011, LME Copper.30Sep2011, LME Copper.03Oct2011, LME Copper.04Oct2011) + 0.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(3), ex3)
    //val ex4 = "((-Sum(((Average(98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT) + 1.50 USD/MT) * 0.40), ((Sum((98.00 USD/MT * 0.10), (98.00 USD/MT * 0.10), (98.00 USD/MT * 0.20), (98.00 USD/MT * 0.10), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.20), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.10)) + 1.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    val ex4 = "((-Sum(((Average(98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT) + 1.50 USD/MT) * 0.40), ((Average(97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT) + 0.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(4), ex4)
    assertEquals(explanation.format(5), ex4)
  }

  //@Test
  def testOptionalPricingExplanationIsSameAsFirstChoice{
    val monthSpec = AveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 8), Quantity(1.5, USD/MT))
    val partialSpec = AveragePricingSpec(LmeSingleIndices.cuCashBid, DateRange(Day(2011, 9, 1), Day(2011, 9, 10)), Quantity(4.5, USD/MT))
    val optionalSpec = OptionalPricingSpec(List(monthSpec, partialSpec), Day(2011, 8, 25), chosenSpec = None)
    val optAssignment = PhysicalMetalAssignment("123", Quantity(100, MT), Lead, Day(2011, 9, 1), optionalSpec, ContractualLocationCode("France"), IncotermCode("CIF"),
      benchmarkDeliveryDay = None, benchmarkCountryCode = Some(NeptuneCountryCode("Italy")), benchmarkIncoTermCode = Some(IncotermCode("CIF")), isPurchase = true, inventoryID = "inventory id", inventoryQuantity = Quantity(90, MT),
      grade = GradeCode("grade"))
    val monthAssignment = PhysicalMetalAssignment("123", Quantity(100, MT), Lead, Day(2011, 9, 1), monthSpec, ContractualLocationCode("France"), IncotermCode("CIF"), benchmarkDeliveryDay = None,
      benchmarkCountryCode = Some(NeptuneCountryCode("Italy")), benchmarkIncoTermCode = Some(IncotermCode("CIF")), isPurchase = true, inventoryID = "inventory id", inventoryQuantity = Quantity(90, MT),
      grade = GradeCode("grade"))
    val optExp = optAssignment.explanation(env)
    val monthExp = monthAssignment.explanation(env)
    for (i <- List(0, 1, 2, 3, 4, 5))
      assertEquals(optExp.format(i), monthExp.format(i))
  }

  //@Test
  def testFixedPrixingSpec{
    val fixedSpec = FixedPricingSpec(Day(2011, 8, 31), List((0.5, Quantity(98, USD/MT)), (0.8, Quantity(103, USD/MT))), Quantity(1.5, USD/MT))
    val assignment = PhysicalMetalAssignment("abc", Quantity(100, MT), Lead, Day(2011, 9, 1), fixedSpec, ContractualLocationCode("France"), IncotermCode("CIF"),
      benchmarkDeliveryDay = None, benchmarkCountryCode = Some(NeptuneCountryCode("Italy")), benchmarkIncoTermCode = Some(IncotermCode("CIF")), isPurchase = true, inventoryID = "inventory id", inventoryQuantity = Quantity(90, MT),
      grade = GradeCode("grade"))
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    assertEquals(explanation.format(1), "((-((Sum((F_0 * 0.5), (F_1 * 0.8)) / 1.3) + Premium) * 100.00 MT) * USD.31Aug2011)")
    val lastExplanation = "((-((Sum((98.00 USD/MT * 0.50), (103.00 USD/MT * 0.80)) / 1.30) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(2), lastExplanation)
    assertEquals(explanation.format(3), lastExplanation)
  }
  
  //@Test
  def testUnknownPrixingSpec{
    val unknownPricingSpec = UnknownPricingSpecification(
      LmeSingleIndices.cuCashBid,
      Month(2011, 8),
      List(
        UnknownPricingFixation(0.2, Quantity(98, USD/MT)),
        UnknownPricingFixation(-0.1, Quantity(98, USD/MT))
      ),
      Day(2011, 8, 31),
      Quantity(1.5, USD/MT)
    )
    val assignment = PhysicalMetalAssignment("123", Quantity(100, MT), Lead, Day(2011, 9, 1), unknownPricingSpec, ContractualLocationCode("France"), IncotermCode("CIF"),
      benchmarkDeliveryDay = None, benchmarkCountryCode = Some(NeptuneCountryCode("Italy")), benchmarkIncoTermCode = Some(IncotermCode("CIF")), isPurchase = true, inventoryID = "inventory id", inventoryQuantity = Quantity(90, MT),
      grade = GradeCode("grade"))
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    assertEquals(explanation.format(1), "((-((Fixed + Unfixed) + Premium) * 100.00 MT) * USD.19Aug2011)")
    assertEquals(explanation.format(2), "((-((Sum((Fix_0 * 0.2), (Fix_1 * -0.1)) + (17Aug2011 * 0.9)) + 1.50 USD/MT) * 100.00 MT) * 1.00)")
    assertEquals(explanation.format(3), "((-((Sum((98.00 USD/MT * 0.20), (98.00 USD/MT * (0.10))) + (LME Copper.19Aug2011 * 0.90)) + 1.50 USD/MT) * 100.00 MT) * 1.00)")
    val lastExplanation = "((-((Sum((98.00 USD/MT * 0.20), (98.00 USD/MT * (0.10))) + (97.00 USD/MT * 0.90)) + 1.50 USD/MT) * 100.00 MT) * 1.00)"
    assertEquals(explanation.format(4), lastExplanation)
    assertEquals(explanation.format(5), lastExplanation)
  }

  @Test
  def testMixedCurrenciesAndVolumes{
    val contractPricingSpec = AveragePricingSpec(
      LmeSingleIndices.cuCashOffer,
      Month(2012, 1),
      Quantity(1.5, EUR/KG)
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
          case _ : FreightParityAtomicKey => Quantity(5, CNY / LB)

        }
      )
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

