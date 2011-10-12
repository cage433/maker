package starling.market

import rules._
import starling.curves.Environment
import starling.daterange.DateRange
import starling.quantity.UOM._
import starling.market.Index._
import starling.calendar.BrentMonth
import starling.quantity.{UOM, Quantity}

case class BrentCFDSpreadIndex(forwardBrent: PublishedIndex, month: BrentMonth) extends MultiIndex(DATED_BRENT + " vs " + forwardBrent) {

  def averagePrice(env: Environment, averagingPeriod: DateRange, pricingRule: SwapPricingRule, priceUOM: UOM, rounding: Option[Int], roundingMethodRule: RoundingMethodRule) = {
    assert(pricingRule == CommonPricingRule, "Only Common Rule makes sense for Brent CFD")
    assert(roundingMethodRule == PerQuoteRule, "Only PerQuoteRule available for CFDs")
    val observationDays = pricingRule.observationDays(calendars, averagingPeriod)

    val prices1 = observationDays.map(env.fixingOrForwardPrice(DATED_BRENT, _))
    val avg1 = checkedConvert(DATED_BRENT, Quantity.average(prices1), priceUOM)

    val prices2 = observationDays.map(env.fixingOrForwardPrice(forwardBrent, _))
    val avg2 = checkedConvert(forwardBrent, Quantity.average(prices2), priceUOM)

    rounding match {
      case Some(dp) => avg1.round(dp) - avg2.round(dp)
      case None => avg1 - avg2
    }
  }

  def precision = Some(Precision(3, 2)) // From tblquotes

  def indexes = Set(DATED_BRENT, forwardBrent)

  def uom = BBL

  def priceUOM = USD / BBL
}

object BrentCFDSpreadIndex {
  lazy val named: Map[String, BrentCFDSpreadIndex] = {
    (1 to 12).map {
      i => {
        val month = new BrentMonth(i)
        val index = Index.publishedIndexFromName("Platts Brent (" + month.monthName + ")")
        val cfd = new BrentCFDSpreadIndex(index, month)
        (cfd.name -> cfd)
      }
    } toMap
  }

  lazy val all = named.values.toList

  def indexFor(brentMonth: BrentMonth): BrentCFDSpreadIndex = indexForOption(brentMonth).get

  def indexForOption(brentMonth: BrentMonth): Option[BrentCFDSpreadIndex] = {
    named.values.find(i => i.month == brentMonth)
  }
}