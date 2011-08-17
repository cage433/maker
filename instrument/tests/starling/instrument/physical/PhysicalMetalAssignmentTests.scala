package starling.instrument.physical

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.market.LmeSingleIndices
import starling.daterange.{Month, Day}
import starling.curves.Environment
import starling.curves.UnitTestingAtomicEnvironment
import starling.curves.ForwardPriceKey
import starling.curves.DiscountRateKey
import starling.curves.FixingKey
import scala.collection.immutable.TreeMap

class PhysicalMetalAssignmentTests extends StarlingTest {
  val marketDay = Day(2011, 8, 10).endOfDay
  val env = Environment(
    new UnitTestingAtomicEnvironment(
      marketDay, 
      {
        case _: ForwardPriceKey => Quantity(97, USD/MT)
        case _: FixingKey => Quantity(98, USD/MT)
        case DiscountRateKey(_, day, _) => new Quantity(math.exp(- 0.1 * day.endOfDay.timeSince(marketDay)))
      }
    )
  )


  @Test
  def testMonthAverageExplanation{
    val monthSpec = MonthAveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 8), Quantity(1.5, USD/MT))
    val assignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), monthSpec)
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    assertEquals(explanation.format(1), "((-(Average(CU Cash Bid.AUGUST 2011) + 1.5 USD/MT) * 100.00 MT) * USD.02Sep2011)")
    val ex = "((-(Average(01Aug2011, 02Aug2011, 03Aug2011, 04Aug2011, 05Aug2011, 08Aug2011, 09Aug2011, 10Aug2011, 11Aug2011, 12Aug2011, 15Aug2011, 16Aug2011, 17Aug2011, 18Aug2011, 19Aug2011, 22Aug2011, 23Aug2011, 24Aug2011, 25Aug2011, 26Aug2011, 29Aug2011, 30Aug2011, 31Aug2011) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(2), ex)
    val ex2 = "((-(Average(LME Copper.03Aug2011 Fixed, LME Copper.04Aug2011 Fixed, LME Copper.05Aug2011 Fixed, LME Copper.08Aug2011 Fixed, LME Copper.09Aug2011 Fixed, LME Copper.10Aug2011 Fixed, LME Copper.11Aug2011 Fixed, LME Copper.12Aug2011 Fixed, LME Copper.15Aug2011, LME Copper.16Aug2011, LME Copper.17Aug2011, LME Copper.18Aug2011, LME Copper.19Aug2011, LME Copper.22Aug2011, LME Copper.23Aug2011, LME Copper.24Aug2011, LME Copper.25Aug2011, LME Copper.26Aug2011, LME Copper.29Aug2011, LME Copper.30Aug2011, LME Copper.31Aug2011, LME Copper.01Sep2011, LME Copper.02Sep2011) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(3), ex2)
    val lastExplanation = "((-(Average(98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT) + 1.50 USD/MT) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(4), lastExplanation)
    assertEquals(explanation.format(5), lastExplanation)
  }

  @Test
  def testPartialAverageExplanation{
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
    val partialSpec = PartialAveragePricingSpec(LmeSingleIndices.cuCashBid, dayFractions, Quantity(1.5, USD/MT))
    val assignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), partialSpec)
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    val ex = "((-(Sum((02Aug2011 * 0.1), (03Aug2011 * 0.1), (04Aug2011 * 0.2), (10Aug2011 * 0.1), (11Aug2011 * 0.1), (16Aug2011 * 0.2), (17Aug2011 * 0.1), (18Aug2011 * 0.1)) + 1.5 USD/MT) * 100.00 MT) * USD.22Aug2011)"
    assertEquals(explanation.format(1), ex)
    val ex1 = "((-(Sum((LME Copper.04Aug2011 Fixed * 0.10), (LME Copper.05Aug2011 Fixed * 0.10), (LME Copper.08Aug2011 Fixed * 0.20), (LME Copper.12Aug2011 Fixed * 0.10), (LME Copper.15Aug2011 * 0.10), (LME Copper.18Aug2011 * 0.20), (LME Copper.19Aug2011 * 0.10), (LME Copper.22Aug2011 * 0.10)) + 1.50 USD/MT) * 100.00 MT) * 1.00)"
    assertEquals(explanation.format(2), ex1)
    val lastExplanation = "((-(Sum((98.00 USD/MT * 0.10), (98.00 USD/MT * 0.10), (98.00 USD/MT * 0.20), (98.00 USD/MT * 0.10), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.20), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.10)) + 1.50 USD/MT) * 100.00 MT) * 1.00)"
    assertEquals(explanation.format(3), lastExplanation)
    assertEquals(explanation.format(4), lastExplanation)
  }


  @Test 
  def testWeightedAverageExplanation{
    val monthSpec = MonthAveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 8), Quantity(1.5, USD/MT))
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
    val partialSpec = PartialAveragePricingSpec(LmeSingleIndices.cuCashBid, dayFractions, Quantity(1.5, USD/MT))
    val weightedAverageSpec = WeightedPricingSpec(List((0.4, monthSpec), (0.6, partialSpec)))
    val assignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), weightedAverageSpec)
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    val ex = "((-Sum(((Average(CU Cash Bid.AUGUST 2011) + 1.5 USD/MT) * 0.4), ((Sum((02Aug2011 * 0.1), (03Aug2011 * 0.1), (04Aug2011 * 0.2), (10Aug2011 * 0.1), (11Aug2011 * 0.1), (16Aug2011 * 0.2), (17Aug2011 * 0.1), (18Aug2011 * 0.1)) + 1.5 USD/MT) * 0.6)) * 100.00 MT) * USD.02Sep2011)"
    assertEquals(explanation.format(1), ex)
    val ex2 = "((-Sum(((Average(01Aug2011, 02Aug2011, 03Aug2011, 04Aug2011, 05Aug2011, 08Aug2011, 09Aug2011, 10Aug2011, 11Aug2011, 12Aug2011, 15Aug2011, 16Aug2011, 17Aug2011, 18Aug2011, 19Aug2011, 22Aug2011, 23Aug2011, 24Aug2011, 25Aug2011, 26Aug2011, 29Aug2011, 30Aug2011, 31Aug2011) + 1.50 USD/MT) * 0.40), ((Sum((LME Copper.04Aug2011 Fixed * 0.10), (LME Copper.05Aug2011 Fixed * 0.10), (LME Copper.08Aug2011 Fixed * 0.20), (LME Copper.12Aug2011 Fixed * 0.10), (LME Copper.15Aug2011 * 0.10), (LME Copper.18Aug2011 * 0.20), (LME Copper.19Aug2011 * 0.10), (LME Copper.22Aug2011 * 0.10)) + 1.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(2), ex2)
    val ex3 = "((-Sum(((Average(LME Copper.03Aug2011 Fixed, LME Copper.04Aug2011 Fixed, LME Copper.05Aug2011 Fixed, LME Copper.08Aug2011 Fixed, LME Copper.09Aug2011 Fixed, LME Copper.10Aug2011 Fixed, LME Copper.11Aug2011 Fixed, LME Copper.12Aug2011 Fixed, LME Copper.15Aug2011, LME Copper.16Aug2011, LME Copper.17Aug2011, LME Copper.18Aug2011, LME Copper.19Aug2011, LME Copper.22Aug2011, LME Copper.23Aug2011, LME Copper.24Aug2011, LME Copper.25Aug2011, LME Copper.26Aug2011, LME Copper.29Aug2011, LME Copper.30Aug2011, LME Copper.31Aug2011, LME Copper.01Sep2011, LME Copper.02Sep2011) + 1.50 USD/MT) * 0.40), ((Sum((98.00 USD/MT * 0.10), (98.00 USD/MT * 0.10), (98.00 USD/MT * 0.20), (98.00 USD/MT * 0.10), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.20), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.10)) + 1.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(3), ex3)
    val ex4 = "((-Sum(((Average(98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 98.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT, 97.00 USD/MT) + 1.50 USD/MT) * 0.40), ((Sum((98.00 USD/MT * 0.10), (98.00 USD/MT * 0.10), (98.00 USD/MT * 0.20), (98.00 USD/MT * 0.10), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.20), (97.00 USD/MT * 0.10), (97.00 USD/MT * 0.10)) + 1.50 USD/MT) * 0.60)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(4), ex4)
    assertEquals(explanation.format(5), ex4)
  }

  @Test
  def testOptionalPricingExplanationIsSameAsFirstChoice{
    val monthSpec = MonthAveragePricingSpec(LmeSingleIndices.cuCashBid, Month(2011, 8), Quantity(1.5, USD/MT))
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
    val partialSpec = PartialAveragePricingSpec(LmeSingleIndices.cuCashBid, dayFractions, Quantity(1.5, USD/MT))
    val optionalSpec = OptionalPricingSpec(List(monthSpec, partialSpec), Day(2011, 8, 25), chosenSpec = None)
    val optAssignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), optionalSpec)
    val monthAssignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), monthSpec) 
    val optExp = optAssignment.explanation(env)
    val monthExp = monthAssignment.explanation(env)
    for (i <- List(0, 1, 2, 3, 4, 5))
      assertEquals(optExp.format(i), monthExp.format(i))
  }

  @Test
  def testFixedPrixingSpec{
    val fixedSpec = FixedPricingSpec(Day(2011, 8, 31), List((0.5, Quantity(98, USD/MT)), (0.8, Quantity(103, USD/MT))))
    val assignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), fixedSpec) 
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    assertEquals(explanation.format(1), "((-(Sum((F_0 * 0.5), (F_1 * 0.8)) / 1.3) * 100.00 MT) * USD.31Aug2011)")
    val lastExplanation = "((-(Sum((98.00 USD/MT * 0.50), (103.00 USD/MT * 0.80)) / 1.30) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(2), lastExplanation)
    assertEquals(explanation.format(3), lastExplanation)
  }
  
  @Test
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
    val assignment = PhysicalMetalAssignment("Lead", Quantity(100, MT), Day(2011, 9, 1), unknownPricingSpec) 
    val explanation = assignment.explanation(env)
    assertEquals(explanation.name, "((-F * Volume) * Discount)")
    assertEquals(explanation.format(1), "((-(Fixed + Unfixed) * 100.00 MT) * USD.02Sep2011)")
    assertEquals(explanation.format(2), "((-(Sum((Fix_0 * 0.2), (Fix_1 * -0.1)) + (17Aug2011 * 0.9)) * 100.00 MT) * 0.99)")
    assertEquals(explanation.format(3), "((-(Sum((98.00 USD/MT * 0.20), (98.00 USD/MT * (0.10))) + (LME Copper.19Aug2011 * 0.90)) * 100.00 MT) * 0.99)")
    val lastExplanation = "((-(Sum((98.00 USD/MT * 0.20), (98.00 USD/MT * (0.10))) + (97.00 USD/MT * 0.90)) * 100.00 MT) * 0.99)"
    assertEquals(explanation.format(4), lastExplanation)
    assertEquals(explanation.format(5), lastExplanation)
  }
}

