package starling.market.rules

import starling.utils.cache.CacheFactory
import java.io.Serializable
import starling.calendar.{BusinessCalendarSet, BusinessCalendar}
import starling.daterange.{Location, DateRange, Day}
import starling.utils.StarlingEnum

sealed trait RoundingMethodRule extends Serializable {
  def name: String

  override def toString = name

  def roundPerQuote: Boolean
  def roundFormula: Boolean
}

object RoundingMethodRule extends StarlingEnum(classOf[RoundingMethodRule], (r:RoundingMethodRule) => r.name, ignoreCase = true) {
  def unapply(s: String) = find(s)

  val perFormula: RoundingMethodRule = PerFormulaRule
  val perQuote: RoundingMethodRule = PerQuoteRule
}

case object PerFormulaRule extends RoundingMethodRule {
  val name = "per Formula"

  def roundPerQuote = false

  def roundFormula = true
}

case object PerQuoteRule extends RoundingMethodRule {
  val name = "per Quote"

  def roundPerQuote = true

  def roundFormula = false
}