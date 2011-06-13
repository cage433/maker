package starling.reports.pivot

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.quantity.RichQuantity._
import starling.utils.ScalaTestUtils._
import starling.utils.QuantityTestUtils._
import starling.market._
import rules.{Precision, NonCommonPricingRule, CommonPricingRule}
import starling.curves._
import starling.market.formula._
import starling.daterange._
import starling.daterange.Day._
import starling.quantity.RichQuantity._
import starling.pivot.PivotQuantity
import starling.utils.{AtomicDatumKeyUtils, StarlingTest}
import starling.instrument.CommoditySwap

class IndexRuleEvaluationTest extends JonTestEnv {

  @Test
  def testOutput {
//    val wti = Index.WTI10
//    val brent = Index.BRT11
//    val index = FuturesSpreadIndex("wti - brt", wti, brent, USD, BBL, Some(Precision(3, 2)))
//    val period = Month(2011, 4)
//    val env = makeEnv(Day(2011, 4, 15).endOfDay)

//    val rows = IndexRuleEvaluation.rows(index, period, NonCommonPricingRule, 2, env)
//    assertTrue(wti.observationDays(period).forall(day => rows.find(r => r.market == wti.market && r.day == day) isDefined))
//    assertTrue(brent.observationDays(period).forall(day => rows.find(r => r.market == brent.market && r.day == day) isDefined))
//
//    rows.map(println)
  }
}