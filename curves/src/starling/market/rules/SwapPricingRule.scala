package starling.market
package rules

import starling.utils.cache.CacheFactory
import java.io.Serializable
import starling.calendar.{BusinessCalendarSet, BusinessCalendar}
import starling.daterange.{Location, DateRange, Day}

sealed trait SwapPricingRule extends Serializable {
  val name: String

  override def toString = name

  def isObservationDay(markets: List[CommodityMarket], day: Day): Boolean

  def isValid(markets: List[CommodityMarket]) = true

  @transient protected lazy val observationDayCache = CacheFactory.getCache("SwapPricingRule", unique = true)

  def observationDays(markets: List[CommodityMarket], period: DateRange): List[Day] = observationDayCache.memoize((period, markets), period.days.filter(isObservationDay(markets, _)).toList)
  
  def calendar(markets: List[CommodityMarket]): BusinessCalendar
}

object SwapPricingRule {
  def unapply(name: String): Option[SwapPricingRule] = {
    if (name.toLowerCase == CommonPricingRule.name.toLowerCase)
      Some(CommonPricingRule)
    else if (name.toLowerCase == NonCommonPricingRule.name.toLowerCase)
      Some(NonCommonPricingRule)
    else
      None
  }
}

case object CommonPricingRule extends SwapPricingRule {
  val name = "Common"

  def isObservationDay(markets: List[CommodityMarket], day: Day) = {
    markets.forall(m => m.isObservationDay(day))
  }

  def calendar(markets: List[CommodityMarket]) = {
    var hols = markets.head.businessCalendar.days
    markets.foreach(m => hols = hols.intersect(m.businessCalendar.days))
    new BusinessCalendarSet("CommonPricing for " + markets, Location.Unknown, hols)
  }
}

case object NonCommonPricingRule extends SwapPricingRule {
  val name = "Non-common"

  def isObservationDay(markets: List[CommodityMarket], day: Day) = {
    markets.exists(m => m.isObservationDay(day))
  }

  def calendar(markets: List[CommodityMarket]) = {
    val hols = markets.map(_.businessCalendar.days).flatten.toSet
    new BusinessCalendarSet("NonCommonPricing for " + markets, Location.Unknown, hols)
  }
}

/**
 * No 'SingleIndex' trades will need a pricing rule. Rules are only necessary when two or more
 * indexes are used.
 */
case object NoPricingRule extends SwapPricingRule {
  val name = "NoRule"

  def isObservationDay(markets: List[CommodityMarket], day: Day) = {
    assert(markets.size == 1, "This swap pricing rule is only valid on single indexes: " + markets)
    markets.exists(m => m.isObservationDay(day))
  }

  def calendar(markets: List[CommodityMarket]) = {
    assert(markets.size == 1, "This swap pricing rule is only valid on single indexes: " + markets)
    markets.head.businessCalendar
  }

  override def isValid(markets: List[CommodityMarket]) = markets.size == 1
}
