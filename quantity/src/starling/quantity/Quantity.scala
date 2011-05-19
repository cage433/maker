package starling.quantity

import starling.quantity.UOM._
import math.{abs => mabs}
import java.io.Serializable
import starling.utils.{MathUtil, Summable}
import starling.utils.ImplicitConversions._
import starling.pivot.{StackTrace, PivotQuantity}
import java.lang.String
import starling.utils.ImplicitConversions._
import java.text.DecimalFormat

class QuantityDouble(d : Double){
  def apply(uom : UOM) = Quantity(d, uom)
}
class QuantityInt(n : Int){
  def apply(uom : UOM) = Quantity(n, uom)
}

/**
 * This type class allows the Numeric implicit enabled methods to be used, like TraversableOnce
 * product and sum methods.
 */
trait QuantityIsNumeric extends Numeric[Quantity] {
  def compare(p1: Quantity, p2: Quantity) = p1 compare p2
  def toDouble(x: Quantity) = throw new IllegalStateException("Can't convert Quantity to double.")
  def toFloat(x: Quantity) = throw new IllegalStateException("Can't convert Quantity to float.")
  def toLong(x: Quantity) = throw new IllegalStateException("Can't convert Quantity to long.")
  def toInt(x: Quantity) = throw new IllegalStateException("Can't convert Quantity to int.")

  def fromInt(x: Int) = Quantity(x.toDouble, UOM.SCALAR)
  def negate(x: Quantity) = -x
  def times(x: Quantity, y: Quantity) = x * y
  def minus(x: Quantity, y: Quantity) = x - y
  def plus(x: Quantity, y: Quantity) = x + y

  override def zero = Quantity(0.0, UOM.NULL)
  override def one = Quantity(1.0, UOM.SCALAR)
}

object Quantity {
  val NULL = Quantity(0, UOM.NULL)
  val ONE = Quantity(1, UOM.SCALAR)
  implicit object QuantityIsNumeric extends QuantityIsNumeric
  implicit object NumericOptionQuantity extends OptionNumeric[Quantity]
  val FormatString = "#,##0.00"
  def fromString(text : String, uom : UOM) = Quantity(new java.lang.Double(text).asInstanceOf[Double], uom)
  def sum(quantities : Iterable[Quantity]) = (Quantity.NULL /: quantities)(_+_)
  def average(quantities : Seq[Quantity]) = quantities match {
    case Nil => throw new Exception("Can't get average of empty sequence")
    case _ => sum(quantities) / quantities.size
  }

  implicit def doubleToScalarQuantity(d : Double) : Quantity = Quantity(d, UOM.SCALAR)
  implicit def integerToScalarQuantity(i : Int) : Quantity = Quantity(i, UOM.SCALAR)
  implicit def doubleToQuantityDouble(d : Double) : QuantityDouble = new QuantityDouble(d)
  implicit def integerToQuantityInt(i : Int) : QuantityInt = new QuantityInt(i)

  def apply(value : Double, uom : UOM) = new Quantity(value, uom)
  def unapply(q : Object) : Option[(Double, UOM)] = {
    if (q.isInstanceOf[Quantity]) {
      val qq = q.asInstanceOf[Quantity]
      Some((qq.value, qq.uom))
    } else None
  }
}
 
class Quantity(val value : Double, val uom : UOM) extends Ordered[Quantity] with Summable[Quantity] with Serializable {
  require(! value.isNaN, "Value is NaN")
  require(! value.isInfinite, "Value is Infinite")
  require(value == 0.0 || uom != UOM.NULL, "Can't have non-zero null quantities - you probably want uom SCALAR")
  override def compare(rhs : Quantity) : Int = {
    if(uom != rhs.uom) {
      // allow people to do Quantity(20, BBL) >= 0 etc.
      assert((isZero && uom.isScalar) || (rhs.isZero && rhs.uom.isScalar), "UOMs not the same, can't compare: " + (uom, rhs.uom))
    }
    if (value < rhs.value)
      -1
    else if (value > rhs.value)
      1
    else 0
  }
  def format(fmt : String, useUOM:Boolean = true, addSpace: Boolean = false) = {
    val uomPart = if (useUOM && !uom.isScalar) " " + uom else ""
    value.format(fmt, addSpace) + uomPart
  }
  def format(decimalFormat: DecimalFormat) = decimalFormat.format(value)
  def formatNoUOM(fmt:String) = format(fmt, false)
  override def toString = format(Quantity.FormatString)
  def toStringWithSpace = format(Quantity.FormatString, addSpace = true)
  def this(value : Double, uom : String) = this(value, UOM.fromString(uom))
  def this(value : Double) = this(value, UOM.SCALAR)

  def pq : PivotQuantity = PivotQuantity(Map(uom -> value), Map[String, List[StackTrace]]())

  def in(otherUOM : UOM)(implicit conv: Conversions = Conversions.default): Option[Quantity] = {
    conv.convert(uom, otherUOM).map((ratio) => this * Quantity(ratio, otherUOM / uom))
  }

  /**
   * Converts to this Quanity's base unit, or None. For example 1KG would become 1000G
   */
  def inBaseUnit: Option[Quantity] = {
    Conversions.baseUOM(uom) match {
      case Some(`uom`) => Some(this)
      case Some(baseUOM) => this in baseUOM
      case _ => None
    }
  }

  def isScalar = uom.isScalar

  def +(rhs: Quantity): Quantity = {
    if (this == Quantity.NULL)
      rhs
    else
      rhs match {
        case Quantity(0.0, UOM.NULL) => this
        case Quantity(x, this.uom) => Quantity(value + x, uom)
        case Quantity(x, otherUOM) => rhs in this.uom match {
          case Some(rhsInThisUOM) => this + rhsInThisUOM
          case _ => throw new IllegalStateException(
            "Attempting to add two quantities with different units " + uom + " : " + rhs.uom + " : " + (this, rhs)
          )
        }
      }
  }

  def * (rhs : Double) : Quantity = Quantity(value * rhs, uom)
  def * (rhs : Quantity) : Quantity = {
//    assert(uom != UOM.NULL && rhs.uom != UOM.NULL, "Can't multiply quantities with null units - you probably should have used the scalar unit")
    Quantity(value * rhs.value, uom * rhs.uom)
  }

  def * (rhs : Percentage) : Quantity = this * rhs.value

  def / (rhs : Double) : Quantity = Quantity(value / rhs, uom)
  /** Divide by another Quantity
   */
  def / (rhs : Quantity) = this * (rhs.invert)


  def abs = Quantity(mabs(value), uom)
  
  def negate = unary_-

  def unary_- : Quantity = Quantity(-value, uom)

  /** subtract another quantity from this
   */
  def - (rhs : Quantity) : Quantity = this + -rhs

  /** return the inversion of this
   */
  def invert : Quantity = {
    if (value == 0)
    	throw new IllegalStateException("Dividing by zero")
  	else
       Quantity(1.0 / value, uom.inverse)
  }

  /** Returns the value after asserting the UOM is as expected
   */
  def checkedValue(expectedUOM : UOM) : Double = {
    assert(expectedUOM == uom, "Expected UOM " + expectedUOM + ", got unit " + uom + ".")
    value
  }
  
  def numeratorUOM = uom.numeratorUOM
  def denominatorUOM = uom.denominatorUOM

  def copy(value:Double = this.value, uom:UOM = this.uom) = Quantity(value, uom)

  override def equals(other: Any) = other match {
    case q : Quantity => q.isAlmostEqual(this, MathUtil.EPSILON)
    case _ => false
  }

  override val hashCode = (value / MathUtil.EPSILON).round.hashCode

  def isAlmostZero : Boolean = value.abs < MathUtil.EPSILON
  def isZero : Boolean = value == 0.0
  def isPositve : Boolean = value >= 0.0
  def isNegative : Boolean = !isPositve
  def isNegativeOrZero: Boolean = value <= 0
  def percentageDifference(other:Quantity) = {
    (this.value, other.value) match {
      case (0, 0) => Percentage(0)
      case (0, _) => Percentage(1)
      case (_, 0) => Percentage(1)
      case _ => Percentage(((this - other).abs / this).value)
    }
  }

  def isAlmostEqual(other : Quantity, tolerance : Double) = {
    uom == other.uom && (
            mabs(value - other.value) <= tolerance.abs ||
            value.isInfinite && other.value.isInfinite ||
            value.isNaN && value.isNaN
            )
  }

  /**
   * The financial 'max with zero' operator
   */
  def ^+() = if (value > 0) this else Quantity(0, uom)

  def sign =
    if (value > 0)
      1.0
    else if (value < 0)
      -1.0
    else 0.0

  def min(rhs : Quantity) = {
    assert(uom == rhs.uom, "UOM mismatch " + uom + " vs " + rhs.uom)
    Quantity(value min rhs.value, uom)
  }

  def named(name: String) = new NamedQuantity("%s" % (name), this)

  def round(dp: Int) = copy(value = MathUtil.roundToNdp(value, dp))
}

case class NamedQuantity(expr: String, quantity: Quantity) extends Quantity(quantity.value, quantity.uom) {
  override def * (rhs: Double)     = guard(isAlmostOne(rhs), new NamedQuantity(binOp('*', rhs), quantity * rhs))
  override def * (rhs: Percentage) = guard(isAlmostOne(rhs), new NamedQuantity(binOp('*', rhs), quantity * rhs))
  override def * (rhs: Quantity)   = guard(rhs == Quantity.ONE, new NamedQuantity(binOp('*', rhs), quantity * rhs))
  override def / (rhs: Double)     = guard(isAlmostOne(rhs), new NamedQuantity(binOp('/', rhs), quantity / rhs))
  override def / (rhs: Quantity)   = guard(rhs == Quantity.ONE, new NamedQuantity(binOp('/', rhs), quantity / rhs))
  override def + (rhs: Quantity)   = new NamedQuantity(binOp('+', rhs), quantity + rhs)
  override def - (rhs: Quantity)   = new NamedQuantity(binOp('-', rhs), quantity - rhs)
  override def unary_-             = new NamedQuantity("-%s"     % (expr), -quantity)
  override def abs                 = new NamedQuantity("abs(%s)" % (expr), quantity.abs)
  override def invert              = new NamedQuantity("(1/%s)"  % (expr), quantity.invert)
  override def negate              = unary_-

  override def pq = new PivotQuantity(Map(uom -> value), Map[String, List[StackTrace]]()) {
    override def explanation = Some(expr)
  }

  private def binOp(op: Char, rhs: Any) = {
    val rhsVal = rhs match {
      case s:NamedQuantity => s.expr
      case quantity:Quantity => {
        val number = {
          val string = quantity.value.toString
          val ePos = string.indexOf('E')
          if (ePos > 0) {
            string.substring(0, ePos) + '×' + "10" + string.substring(ePos + 1).map(superscripts)
          } else {
            string
          }
        }

        val uom = if (quantity.isScalar) "" else " " + quantity.uom
        number + uom
      }
      case other => rhs.toString.replace("0.00", "whoops")
    }

    "(%s %c %s)" % (expr, op, rhsVal)
  }

  private val superscripts = Map('-' -> '⁻',
    '0' -> '⁰', '1' -> '¹', '2' -> '²', '3' -> '³', '4' -> '⁴', '5' -> '⁵', '6' -> '⁶', '7' -> '⁷', '8' -> '⁸', '9' -> '⁹')

  private def isAlmostOne(value: Double): Boolean          = (value - 1).abs < MathUtil.EPSILON
  private def isAlmostOne(percentage: Percentage): Boolean = isAlmostOne(percentage.value)
  private def guard(condition: Boolean, fn: => NamedQuantity) = if (condition) this else fn
}
