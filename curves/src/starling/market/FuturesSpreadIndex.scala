package starling.market

import formula.FormulaIndexes
import rules.{Precision, SwapPricingRule}
import starling.utils.CaseInsensitive
import starling.quantity.UOM._
import starling.daterange.DateRange
import starling.curves.Environment
import starling.quantity.{Quantity, UOM}

/**
 * The definition of the spread is market1 - market2
 */
// TODO [26 Jan 2011] this needs to be renamed to SpreadIndex after Alex's refactor
case class FuturesSpreadIndex(spreadName: CaseInsensitive, index1: SingleIndex, index2: SingleIndex, ccy: UOM, uom: UOM, precision: Option[Precision]) extends MultiIndex(spreadName) {
  def indexes = Set(index1, index2)

  def priceUOM = ccy / uom

  def averagePrice(env: Environment, averagingPeriod: DateRange, rule: SwapPricingRule, priceUOM: UOM) = {
    val observationDays1 = index1.observationDays(averagingPeriod).intersect(rule.observationDays(calendars, averagingPeriod))
    val prices1 = observationDays1.map(env.fixingOrForwardPrice(index1, _))
    val avg1 = checkedConvert(index1, Quantity.average(prices1), priceUOM)
    
    val observationDays2 = index2.observationDays(averagingPeriod).intersect(rule.observationDays(calendars, averagingPeriod))
    val prices2 = observationDays2.map(env.fixingOrForwardPrice(index2, _))
    val avg2 = checkedConvert(index2, Quantity.average(prices2), priceUOM)

    avg1 - avg2
  }
}

object FormulaIndexList {
  var indexes: Option[FormulaIndexes] = None

  def set(i: Option[FormulaIndexes]) = {
    indexes = i
  }

  lazy val formulaIndexes = indexes match {
    case Some(i) => i.indexes
    case None => throw new Exception("Formula Indexes not set up")
  }

  lazy val eaiQuoteMap = indexes match {
    case Some(i) => i.eaiQuoteMap
    case None => throw new Exception("Formula Indexes not set up")
  }
}

object FuturesSpreadIndex{
  /**
   * Spread Indexes
   */
  val GAS_OIL_CRACK = FuturesSpreadIndex(
    "Gas Oil Crack",
    FuturesFrontPeriodIndex.GO11,
    FuturesFrontPeriodIndex.BRT11,
    USD,
    BBL,
    Some(Precision(3, 2))
  )
  val UNL_87_USGC_PIPELINE_VS_NYMEX_WTI = FuturesSpreadIndex(
    "Unl 87 USGC Pipeline vs NYMEX WTIt",
    PublishedIndex.UNL_87_USGC_PIPELINE,
    FuturesFrontPeriodIndex.WTI10,
    USD,
    BBL,
    Some(Precision(4, 2))
  )

  val spreadIndexes: List[FuturesSpreadIndex] = List(GAS_OIL_CRACK, UNL_87_USGC_PIPELINE_VS_NYMEX_WTI)
}

