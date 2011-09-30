package starling.quantity

import starling.quantity.UOM._
import math.{abs => mabs}
import java.io.Serializable
import utils.Summable
import starling.utils.{MathUtil}
import starling.utils.ImplicitConversions._
import starling.pivot.{StackTrace, PivotQuantity}
import java.lang.String
import starling.utils.ImplicitConversions._
import starling.utils.Pattern._
import java.text.DecimalFormat

object Quantity {
  val Regex = """([0-9.,]*) (.*)""".r
  val mc = java.math.MathContext.DECIMAL128

  val NULL = Quantity(0, UOM.NULL)
  val ONE = Quantity(1, UOM.SCALAR)
  implicit object QuantityIsNumeric extends QuantityIsNumeric
  implicit object NumericOptionQuantity extends OptionNumeric[Quantity]
  val FormatString = "#,##0.00"
  def fromString(text : String, uom : UOM) = Quantity(new java.lang.Double(text).asInstanceOf[Double], uom)
  val Parse: Extractor[String, Quantity] =
    Extractor.from[String](_.partialMatch { case Regex(value, UOM.Parse(uom)) => fromString(value, uom) })

  private def sumAsBigDecimal(quantities: Iterable[Quantity]): (BigDecimal, UOM) = {
    if(quantities.nonEmpty) {
      val uoms = quantities.map(_.uom).toSet
      if(uoms.size == 1) {
        val sum = (BigDecimal("0") /: quantities.map(q => BigDecimal(q.value)))(_ + _)
        (sum, uoms.head)
      } else {
        val sum = (Quantity.NULL /: quantities)(_ + _)
        (BigDecimal(sum.value), sum.uom)
      }
    } else {
      (BigDecimal("0"), UOM.NULL)
    }
  }

  def average(quantities : Seq[Quantity]) = quantities match {
    case Nil => throw new Exception("Can't get average of empty sequence")
    case _ => {
      val (sum, uom) = sumAsBigDecimal(quantities)
      val result = Quantity((sum(mc) / quantities.size).toDouble, uom)

      if (quantities.forall(_.isInstanceOf[NamedQuantity])) {
        FunctionNamedQuantity("Average", quantities.toList.asInstanceOf[List[NamedQuantity]], result)
      } else {
        result
      }
    }
  }

  def sum(quantities : Iterable[Quantity]) = quantities match {
    case Nil => NULL
    case _ => {
      val (sum, uom) = sumAsBigDecimal(quantities)
      val result = Quantity(sum(mc).toDouble, uom)

      if (quantities.forall(_.isInstanceOf[NamedQuantity])) {
        FunctionNamedQuantity("Sum", quantities.toList.asInstanceOf[List[NamedQuantity]], result)
      } else {
        result
      }
    }
  }

  def sumNamed(quantities : Iterable[NamedQuantity]):NamedQuantity = quantities match {
    case Nil => NamedQuantity.NULL
    case _ => {
      val (sum, uom) = sumAsBigDecimal(quantities)
      val result = Quantity(sum(mc).toDouble, uom)
      FunctionNamedQuantity("Sum", quantities.toList.asInstanceOf[List[NamedQuantity]], result)
    }
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
  //require(value == 0.0 || uom != UOM.NULL, "Can't have non-zero null quantities - you probably want uom SCALAR")
  override def compare(rhs : Quantity) : Int = {
    if(uom != rhs.uom) {
      // allow people to do Quantity(20, BBL) >= 0 etc.
      assert((isZero && uom.isScalar) || (rhs.isZero && rhs.isScalar), "UOMs not the same, can't compare: " + (uom, rhs.uom))
    }
    if (value < rhs.value)
      -1
    else if (value > rhs.value)
      1
    else 0
  }
  def format(fmt : String, useUOM:Boolean = true, addSpace: Boolean = false, negativeBrackets: Boolean = true) = {
    val uomPart = if (useUOM && !isScalar && !uom.isNull) " " + uom else ""
    value.format(fmt, addSpace, negativeBrackets) + uomPart
  }
  def format(decimalFormat: DecimalFormat) = decimalFormat.format(value)
  def formatNoUOM(fmt:String) = format(fmt, false)
  override def toString = format(Quantity.FormatString)
  def toStringAllDecimalPlaces(negativeBrackets:Boolean=true) = format("#,##0.00#######################################", negativeBrackets = negativeBrackets)
  def toStringWithSpace = format(Quantity.FormatString, addSpace = true)
  def this(value : Double, uom : String) = this(value, UOM.fromString(uom))
  def this(value : Double) = this(value, UOM.SCALAR)

  def pq : PivotQuantity = PivotQuantity(Map(uom -> value), Map[String, List[StackTrace]]())

  def in(otherUOM : UOM)(implicit conv: Conversions = Conversions.default): Option[Quantity] = {
    conv.convert(uom, otherUOM).map((ratio) => Quantity((this.value * ratio).toDouble, otherUOM))
  }

  /**
   * E.g. 1000 Cent/KG to .01 USD/G
   * used for equals and hashcode
   */
  def inBaseUOM = {
    this.inUOM(uom.inBaseUnit)
  }

  /**
   * E.g. 100 Cent would convert to 1 USD
   */
  def inBaseCurrency = {
    assert(this.uom.isCurrency, "Can't convert this to currency: " + this)
    this.inUOM(uom.inBaseCurrency)
  }

  def inUOM(uom: UOM)(implicit conv: Conversions = Conversions.default): Quantity = {
    in(uom)(conv).getOrElse(throw new Exception(this + ": Couldn't convert from " + this + " to " + uom))
  }

  def isScalar = uom.isScalar

  def +(rhs: Quantity): Quantity = {
    if (this == Quantity.NULL)
      rhs
    else
      rhs match {
        case Quantity(0.0, UOM.NULL) => this
        case Quantity(x, this.uom) => Quantity(value + x, uom)
        case Quantity(x, otherUOM) => this.uom.plus(otherUOM) match {
          case Some(conversion) => this + rhs.copy(value = rhs.value * conversion.toDouble, uom = this.uom)
          case _ => throw new IllegalStateException(
            "Attempting to add two quantities with different units " + uom + " : " + rhs.uom + " : " + (this, rhs)
          )
        }
      }
  }

  def * (rhs : Double) : Quantity = Quantity(value * rhs, uom)

  def *(rhs: Quantity): Quantity = {
    //    assert(uom != UOM.NULL && rhs.uom != UOM.NULL, "Can't multiply quantities with null units - you probably should have used the scalar unit")
    (uom.mult(rhs.uom)) match {
      case (newUOM, c) => Quantity((c * value * rhs.value).toDouble, newUOM)
    }
  }

  def * (rhs : Percentage) : Quantity = this * rhs.value
  def / (rhs : Double) : Quantity = Quantity(value / rhs, uom)
  def / (rhs : Quantity) = this * (rhs.invert)
  def abs = Quantity(mabs(value), uom)
  def negate = unary_-
  def unary_- : Quantity = Quantity(-value, uom)
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
    this in expectedUOM match {
      case Some(q) => q.value
      case None => throw new java.lang.AssertionError("Expected UOM " + expectedUOM + ", got unit " + uom + ".")
    }
  }

  def numeratorUOM = uom.numeratorUOM
  def denominatorUOM = uom.denominatorUOM

  def copy(value:Double = this.value, uom:UOM = this.uom) = Quantity(value, uom)

  override def equals(other: Any) = other match {
    case Quantity(this.value, this.uom) => true
    case Quantity(_, this.uom) => false // different values, same uom - false
    case q @ Quantity(_, UOM(this.uom.uType, _, _)) => compareInBaseUOM(q.asInstanceOf[Quantity]) // same main type, could be equal
    case _ => false
  }

  private def compareInBaseUOM(other: Quantity) = {
    val thisBase = this.inBaseUOM
    other.inBaseUOM match {
      case Quantity(thisBase.value, thisBase.uom) => true
      case _ => false
    }
  }

  def isAlmostEqual(other : Quantity, tolerance : Double) = {
    val thisBase = this.inBaseUOM
    val otherBase = other.inBaseUOM
    val tol = Quantity(tolerance, uom).inBaseUOM
    thisBase.uom == otherBase.uom && (
            mabs(thisBase.value - otherBase.value) <= tol.value.abs ||
            thisBase.value.isInfinite && otherBase.value.isInfinite ||
            thisBase.value.isNaN && otherBase.value.isNaN
            )
  }

  override lazy val hashCode = inBaseUOM.value.hashCode

  def isAlmostZero : Boolean = value.abs < MathUtil.EPSILON
  def isZero : Boolean = value == 0.0
  def isPositve : Boolean = value >= 0.0
  def isNegative : Boolean = !isPositve
  def isNegativeOrZero: Boolean = value <= 0
  def isNonNegative: Boolean = isPositve || isZero
  def percentageDifference(other:Quantity) = {
    (this.value, other.value) match {
      case (0, 0) => Percentage(0)
      case (0, _) => Percentage(1)
      case (_, 0) => Percentage(1)
      case _ => Percentage(((this - other).abs / this).value)
    }
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

  def named(name: String) = SimpleNamedQuantity(name, this)

  def round(dp: Int) = copy(value = MathUtil.roundToNdp(value, dp))
}

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

abstract class NamedQuantity(val quantity : Quantity) extends Quantity(quantity.value, quantity.uom){
  import NamedQuantity._
  def format(level : Int) : String
  def name : String = format(0)
  override def * (rhs: Double)     = guard(isAlmostOne(rhs), BinOpNamedQuantity("*", this, asNamedQuantity(rhs), quantity * rhs))
  override def * (rhs: Percentage) = guard(isAlmostOne(rhs), BinOpNamedQuantity("*", this, asNamedQuantity(rhs), quantity * rhs))
  override def * (rhs: Quantity)   = guard(rhs == Quantity.ONE, BinOpNamedQuantity("*", this, asNamedQuantity(rhs), quantity * rhs))
  override def / (rhs: Double)     = guard(isAlmostOne(rhs), BinOpNamedQuantity("/", this, asNamedQuantity(rhs), quantity / rhs))
  override def / (rhs: Quantity)   = guard(rhs == Quantity.ONE, BinOpNamedQuantity("/", this, asNamedQuantity(rhs), quantity / rhs))
  override def + (rhs: Quantity)   = BinOpNamedQuantity("+", this, asNamedQuantity(rhs), quantity + rhs)
  override def - (rhs: Quantity)   = BinOpNamedQuantity("-", this, asNamedQuantity(rhs), quantity - rhs)
  override def unary_-             = NegateNamedQuantity(this)
  override def abs                 = FunctionNamedQuantity("abs", List(this), quantity.abs)
  override def invert              = InvertNamedQuantity(this)
  override def negate              = unary_-
  override def in(otherUOM : UOM)(implicit conv: Conversions = Conversions.default): Option[NamedQuantity] = {
    conv.convert(uom, otherUOM).map((ratio) => if (ratio == 1.0) {
      this
    }else{
      val cq = Quantity((this.value * ratio).toDouble, otherUOM)
      (uom.div(otherUOM)) match {
        case (UOM.SCALAR, _) => FunctionNamedQuantity("Convert " + uom + " To " + otherUOM, List(this), cq, custom = true)
        case _ => this * (SimpleNamedQuantity("Conversion", Quantity(ratio.toDouble, otherUOM/uom)))
      }
    })
  }

  override def inUOM(uom: UOM)(implicit conv: Conversions): NamedQuantity = {
    in(uom)(conv) match {
      case Some(beqv) => beqv
      case None => throw new Exception(this + ": Couldn't convert from " + this + " to " + uom)
    }
  }

  private def isAlmostOne(value: Double): Boolean          = (value - 1).abs < MathUtil.EPSILON
  private def isAlmostOne(percentage: Percentage): Boolean = isAlmostOne(percentage.value)
  private def guard(condition: Boolean, fn: => NamedQuantity) = if (condition) this else fn

  override def round(dp:Int) = RoundedNamedQuantity(this, dp)
}

object NamedQuantity{
  val NULL = SimpleNamedQuantity("No name", Quantity.NULL)
  def asNamedQuantity(x : Any) : NamedQuantity = x match {
    case s:NamedQuantity => s
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
      SimpleNamedQuantity(number + uom, quantity)
    }
    case d : Double => SimpleNamedQuantity(d.toString, new Quantity(d))
    case p : Percentage => {
      val v = p.decimalValue
      SimpleNamedQuantity(p.toShortString, new Quantity(v))
    }
    case other => throw new Exception("Unexpected value " + x)
  }
  private val superscripts = Map('-' → '⁻',
    '0' → '⁰', '1' → '¹', '2' → '²', '3' → '³', '4' → '⁴', '5' → '⁵', '6' → '⁶', '7' → '⁷', '8' → '⁸', '9' → '⁹')
}
case class BinOpNamedQuantity(op : String, lhs : NamedQuantity, rhs : NamedQuantity, result : Quantity) extends NamedQuantity(result){
  def format(level : Int) = "(" + lhs.format(level) + " " + op + " " + rhs.format(level) + ")"
}
case class FunctionNamedQuantity(functionName : String, parameters : List[NamedQuantity], result : Quantity, custom : Boolean=false)
        extends NamedQuantity(result) {
  def format(level:Int) = functionName + parameters.map(_.format(level)).mkString("(", ", ", ")")
}

case class InvertNamedQuantity(qty : NamedQuantity) extends NamedQuantity(qty.quantity.invert){
  def format(level:Int) = "(1/" + qty.format(level) + ")"
}
case class NegateNamedQuantity(qty : NamedQuantity) extends NamedQuantity(qty.quantity.negate){
  def format(level:Int) = "-" + qty.format(level)
}

case class SimpleNamedQuantity(override val name : String, qty : Quantity) extends NamedQuantity(qty){
  def format(level:Int) = {
    if (level == 0){
      name
    } else {
      qty match {
        case nq : NamedQuantity => nq.format(level - 1)
        case _ => qty.toString
      }
    }
  }
}

case class RoundedNamedQuantity(orig:NamedQuantity, dp:Int) extends NamedQuantity(orig.quantity.round(dp)) {
  def format(level:Int) = {
    "Round(" + orig.format(level) + ", " + dp + ")"
  }
}

case class PercentageNamedQuantity(override val name : String, p : Percentage) extends NamedQuantity(NamedQuantity.asNamedQuantity(p)){
  def format(level:Int) = {
    if (level == 0){
      name
    } else {
      quantity match {
        case nq : NamedQuantity => nq.format(level - 1)
        case _ => quantity.toString
      }
    }
  }
}
