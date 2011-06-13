package starling.market

import rules.{Precision, CommonPricingRule, SwapPricingRule}
import starling.curves.Environment
import starling.daterange.DateRange
import PublishedIndex._
import starling.quantity.UOM._
import starling.calendar.BrentMonth
import starling.quantity.{UOM, Quantity}

case class BrentCFDSpreadIndex(forwardBrent: PublishedIndex) extends MultiIndex(DATED_BRENT + " vs " + forwardBrent) {
  def averagePrice(env: Environment, averagingPeriod: DateRange, rule: SwapPricingRule, priceUOM: UOM) = {
    assert(rule == CommonPricingRule, "Only Common Rule makes sense for Brent CFD")
    val observationDays = rule.observationDays(calendars, averagingPeriod)

    val prices1 = observationDays.map(env.fixingOrForwardPrice(DATED_BRENT, _))
    val avg1 = checkedConvert(DATED_BRENT, Quantity.average(prices1), priceUOM)

    val prices2 = observationDays.map(env.fixingOrForwardPrice(forwardBrent, _))
    val avg2 = checkedConvert(forwardBrent, Quantity.average(prices2), priceUOM)

    avg1 - avg2
  }

  def precision = Some(Precision(3, 2)) // From tblquotes

  def indexes = Set(DATED_BRENT, forwardBrent)

  def uom = BBL

  def priceUOM = USD / BBL

  lazy val brentMonth: BrentMonth = PLATTS_BRENT.find(_._2 == forwardBrent) match {
    case Some((month, _)) => month
    case _ => throw new Exception("Invalid index? " + forwardBrent)
  }
}

object BrentCFDSpreadIndex {
  lazy val named: Map[String, BrentCFDSpreadIndex] = {
    PublishedIndex.PLATTS_BRENT.values.map(i => {
      val index = BrentCFDSpreadIndex(i)
      (index.name.s -> index)
    }).toMap
  }

  def indexFor(brentMonth: BrentMonth): Option[BrentCFDSpreadIndex] = {
    named.values.find(i => i.brentMonth == brentMonth)
  }
}