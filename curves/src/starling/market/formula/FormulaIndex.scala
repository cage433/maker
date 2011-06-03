package starling.market
package formula

import starling.utils.CaseInsensitive
import starling.curves.Environment
import starling.utils.cache.CacheFactory
import starling.daterange.{TenorType, Day, DateRange}
import formula.Formula.MktInterp
import com.eteks.parser.{CompilationException, CalculatorParser, DoubleInterpreter, DefaultSyntax}
import rules.{Precision, NonCommonPricingRule, SwapPricingRule}
import starling.quantity.{Conversions, UOM, Quantity}

class InvalidFormulaIndexException(msg: String, t: Throwable) extends Exception(msg, t)
class InvalidFormulaException(msg: String, t: Throwable) extends Exception(msg, t)

/**
 * An index based on a given formula. The formula is usually something like A - B. Where A and B are indexes (and
 * may be FormulaIndexes)
 */
case class FormulaIndex(formulaName: CaseInsensitive, formula: Formula, ccy: UOM, uom: UOM, precision: Option[Precision], conversion: Option[Conversions]) extends MultiIndex(formulaName) {
  def indexes = formula.indexes

  def formulaString = formula.toString

  def averagePrice(env: Environment, averagingPeriod: DateRange, rule: SwapPricingRule, priceUOM: UOM) = {
    formula.price(priceUOM) {
      case index: SingleIndex => {
        val observationDays = index.observationDays(averagingPeriod).intersect(rule.observationDays(markets, averagingPeriod))
        val prices = observationDays.map(index.fixingOrForwardPrice(env, _))
        checkedConvert(index, Quantity.average(prices), priceUOM)
      }
      case _ => throw new Exception("Couldn't work out price for formula index on complex indices: " + this)
    }
  }

  def priceUOM = ccy / uom

  def isValid: Boolean = try {
    indexes
    true
  } catch {
    case e: UnknownIndexException => false
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
    case e: CompilationException => throw new InvalidFormulaIndexException("Invalid formula index: ", e)
  }
}

object Formula {

  object Syn extends DefaultSyntax {
    override def getCommonFunctionKey(commonFunction: String) = commonFunction.toLowerCase match {
      case "mkt" => "MKT"
      case _ => super.getCommonFunctionKey(commonFunction)
    }
  }

  class MktInterp(priceUOM: UOM, pr: (SingleIndex) => Quantity) extends DoubleInterpreter {
    override def getCommonFunctionValue(commonFunctionKey: AnyRef, param: AnyRef) = commonFunctionKey match {
      case "MKT" => {
        val quoteID = param.toString.toDouble
        val index = Index.singleIndexFromEAIQuoteID(quoteID.toInt)
        new java.lang.Double(pr(index).checkedValue(priceUOM))
      }
      case _ => super.getCommonFunctionValue(commonFunctionKey, param)
    }
  }

}

/**
 * A formula can give a price and can list the indexes that make it up.
 */
case class Formula(formula: String) {

  import Formula._

  def price(priceUOM: UOM)(pr: (Index) => Quantity): Quantity = {
    val parser = new CalculatorParser(Syn)
    val mktInterp = new MktInterp(priceUOM, pr)
    val value1 = try {
      parser.computeExpression(formula, mktInterp).asInstanceOf[Double]
    }
    catch {
      case e: CompilationException => {
        throw new InvalidFormulaException("Error compiling formula: " + formula, e)
      }
    }
    Quantity(value1, priceUOM)
  }

  def indexes: Set[SingleIndex] = {
    val parser = new CalculatorParser(Syn)
    var indexes = Set[SingleIndex]()
    val mktInterp = new MktInterp(UOM.NULL, index => {
      indexes += index
      Quantity.NULL
    })
    val value1 = parser.computeExpression(formula.toString, mktInterp)
    indexes
  }
}
