package starling.market
package rules

import starling.utils.cache.CacheFactory
import java.io.Serializable
import starling.calendar.{BusinessCalendarSet, BusinessCalendar}
import starling.daterange.{Location, DateRange, Day}

sealed trait SwapPricingRule extends Serializable {
  val name: String

  override def toString = name

  def isObservationDay(calendars: Iterable[BusinessCalendar], day: Day): Boolean

  def isValid(calendars: Iterable[BusinessCalendar]) = true

  @transient protected lazy val observationDayCache = CacheFactory.getCache("SwapPricingRule", unique = true)

  def observationDays(calendars: Iterable[BusinessCalendar], period: DateRange): List[Day] = observationDayCache.memoize((period, calendars), period.days.filter(isObservationDay(calendars, _)).toList)
  
  def calendar(calendars: Iterable[BusinessCalendar]): BusinessCalendar
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

  def isObservationDay(calendars: Iterable[BusinessCalendar], day: Day) = {
    calendars.forall(m => m.isBusinessDay(day))
  }

  def calendar(calendars: Iterable[BusinessCalendar]) : BusinessCalendar = {
    new BusinessCalendar{
      val name = "Common Pricing for " + calendars.map(_.name)
      def location = Location.Unknown
      def isHoliday(day: Day) = calendars.forall(_.isHoliday(day))
    }
  }
}

case object NonCommonPricingRule extends SwapPricingRule {
  val name = "Non-common"

  def isObservationDay(calendars: Iterable[BusinessCalendar], day: Day) = {
    calendars.exists(m => m.isBusinessDay(day))
  }
  def calendar(calendars: Iterable[BusinessCalendar]) : BusinessCalendar = {
    new BusinessCalendar{
      val name = "Non Common Pricing for " + calendars.map(_.name)
      def location = Location.Unknown
      def isHoliday(day: Day) = calendars.exists(_.isHoliday(day))
    }
  }

}

/**
 * No 'SingleIndex' trades will need a pricing rule. Rules are only necessary when two or more
 * indexes are used.
 */
case object NoPricingRule extends SwapPricingRule {
  val name = "NoRule"

  def isObservationDay(calendars: Iterable[BusinessCalendar], day: Day) = {
    assert(calendars.size == 1, "This swap pricing rule is only valid on single calendars: " + calendars)
    calendars.exists(m => m.isBusinessDay(day))
  }

  def calendar(calendars: Iterable[BusinessCalendar]) = {
    assert(calendars.size == 1, "This swap pricing rule is only valid on single calendars: " + calendars)
    calendars.head
  }

  override def isValid(calendars: Iterable[BusinessCalendar]) = calendars.size == 1
}
