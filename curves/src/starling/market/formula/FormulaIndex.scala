package starling.market
package formula

import starling.utils.CaseInsensitive
import starling.curves.Environment
import starling.utils.cache.CacheFactory
import starling.daterange.{TenorType, Day, DateRange}
import starling.quantity._
import rules.{RoundingMethodRule, Precision, NonCommonPricingRule, SwapPricingRule}

class InvalidFormulaIndexException(msg: String, t: Throwable) extends Exception(msg, t)
class InvalidFormulaException(msg: String, t: Throwable) extends Exception(msg, t)

/**
 * An index based on a given formula. The formula is usually something like A - B. Where A and B are indexes (and
 * may be FormulaIndexes)
 */
case class FormulaIndex(formulaName: String, formula: Formula, ccy: UOM, uom: UOM, precision: Option[Precision], conversion: Option[Conversions], override val eaiQuoteID: Option[Int])
  extends MultiIndex(formulaName) {
  lazy val indexes = formula.indexes

  def formulaString = formula.toString

  def averagePrice(env: Environment, averagingPeriod: DateRange, rule: SwapPricingRule,
                   priceUOM: UOM, rounding: Option[Int], roundingMethodRule: RoundingMethodRule) = {
    val naming = env.namingPrefix.isDefined

    def round(q: Quantity) = rounding match {
      case Some(dP) => q.round(dP)
      case _ => q
    }

    def priceForIndex(i: Index) = i match {
      case index: SingleIndex =>
        val observationDays = index.observationDays(averagingPeriod).intersect(rule.observationDays(calendars, averagingPeriod))
        val prices = observationDays.map(env.fixingOrForwardPrice(index, _))
        val price = Quantity.average(prices) match {
          case nq: NamedQuantity => {
            assert(naming)
            SimpleNamedQuantity("Average(" + index + "." + averagingPeriod + ")", nq)
          }
          case q: Quantity => q
        }
        val converted = checkedConvert(index, price, priceUOM)
        converted
      case _ => throw new Exception("Couldn't work out price for formula index on complex indices: " + this)
    }

    def quoteRounding(q: Quantity): Quantity = if (roundingMethodRule.roundPerQuote) {
      round(q)
    } else {
      q
    }

    val evaledPrice = formula.price(naming, priceForIndex, quoteRounding)
    
    if (roundingMethodRule.roundFormula) {
      round(evaledPrice)
    } else {
      evaledPrice
    }
  }

  def priceUOM = ccy / uom

  def isValid: Boolean = try {
    indexes
    true
  } catch {
    case e: UnknownIndexException => false
    case e: InvalidFormulaException => false
    case e: InvalidFormulaIndexException => false
  }

  override def convert(value: Quantity, uom: UOM) = {
    conversion match{
      case Some(c) => value.in(uom)(c)
      case None => super.convert(value, uom)
    }
  }

  /**
   * Verify that we have all the indexes we need for this formula.
   */
  def verify: this.type = try {
    indexes
    this
  } catch {
    case e@UnknownIndexException(_, Some(eaiQuoteID)) => throw new InvalidFormulaIndexException("Invalid formula index: unknown base indexes: " + eaiQuoteID, e)
    case e: UnknownIndexException => throw new InvalidFormulaIndexException("Invalid formula index: unknown base indexes", e)
    case e: IllegalArgumentException => throw new InvalidFormulaIndexException("Invalid formula index: ", e)
  }
}

/**
 * A formula can give a price and can list the indexes that make it up.
 */
case class Formula(formula: String) {
  def canParse: Boolean = try {
    new FormulaParser(Map("mkt" → identity _, "quote" → identity _), false).eval(formula)
    true
  } catch { case _ => false }

  //require(canParse, "Cannot parse: " + formula)

  import Formula._

  def price(named: Boolean, price: (Index) => Quantity, quote: (Quantity) => Quantity): Quantity = {
    def mkt(eaiQuoteID: Quantity): Quantity = {
      assert(eaiQuoteID.isScalar)
      val index = Index.singleIndexFromEAIQuoteID(eaiQuoteID.value.toInt)
      price(index)
    }
    val parser = new FormulaParser(Map("mkt" -> mkt, "quote" -> quote), named)
    try {
      val evaled = parser.eval(formula)
      evaled
    } catch {
      case e: IllegalArgumentException => {
        throw new InvalidFormulaException("Error in formula: " + formula, e)
      }
    }
  }

  lazy val indexes: Set[SingleIndex] = {
    var indexes = Set[SingleIndex]()
    price(false, {
      index: Index => {
        indexes ++= index.indexes
        Quantity.NULL
      }
    }, {
      q: Quantity => q
    })
    indexes
  }

  override def toString = formula
}
