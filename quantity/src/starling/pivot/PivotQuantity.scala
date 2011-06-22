package starling.pivot

import math.abs
import java.io.Serializable
import starling.quantity._
import collection.immutable.Map
import starling.utils.{Named, StarlingEnum, StackTraceToString}

case class PivotPercentage(percent:Option[Percentage]) {
  override def toString = percent match {
    case None => "E"
    case Some(p) => p.toString
  }
}

//We don't want to serialize the StackTraces as exceptions can sometimes contain references to
//unserializable classes (eg CommodityMarket) so we turn exceptions into strings
case class StackTrace(message:String, stackTrace:String)
object StackTrace {
  def apply(t:Throwable) = {
    val message = if (t.getMessage == null) t.toString else t.getMessage
    new StackTrace(message, StackTraceToString.string(t))
  }
}

case class ErrorState(name: String) extends Named {
  import ErrorState._

  def fold[A](g: => A, w: => A, e: => A) = if (this == Warning) w else if (this == Error) e else g
}

object ErrorState extends StarlingEnum(classOf[ErrorState]) {
  val Good    = ErrorState("Good")
  val Warning = ErrorState("Warning")
  val Error   = ErrorState("Error")

  def apply(hasWarnings: Boolean, hasErrors: Boolean): ErrorState = if (hasErrors) Error else if (hasWarnings) Warning else Good
}

case class PivotQuantity(values:Map[UOM,Double], errors:Map[String,List[StackTrace]], warning:Option[String]=None) extends Serializable {
  def this(values:Map[UOM,Double], errors:Set[Throwable]) = this(values, Map() ++ PivotQuantity.throwableToError(errors))
  def this(value:Double, errors:Set[Throwable]) = this(Map(UOM.SCALAR->value), errors)
  def this(value:Double, uom:UOM) = this(Map(uom->value), Map[String,List[StackTrace]]())
  def this(throwables:Set[Throwable]) = this(Map[UOM,Double](), throwables)
  def this(throwable:Throwable) = this(Set(throwable))
  def copyErrors(errors: Set[Throwable]) = copy(errors = PivotQuantity.throwableToError(errors))
  def filterNulls : PivotQuantity = PivotQuantity(Map() ++ values.filterKeys(_ != UOM.NULL), errors)
  def +(other:Option[PivotQuantity]): PivotQuantity = other.map(_ + this).getOrElse(this)
  def +(other:PivotQuantity) = {
    val onlyInThat = other.values -- values.keySet
    new PivotQuantity(
      Map() ++ onlyInThat ++ (for((uom,value) <- values) yield uom->(other.values.getOrElse(uom, 0.0) + value)),
      errors ++ other.errors
    ).filterNulls
  }
  def -(other: PivotQuantity):PivotQuantity = this.+(other * -1.0)
  def /(other:Quantity) = {
    new PivotQuantity(
      values.map{
        case (uom, value) =>
          if (value == 0 && other.value == 0){
            uom / other.uom -> 0.0
          } else{
            val q = Quantity(value, uom) / other
            q.uom -> q.value
          }
      },
      errors)
  }
  def uoms:Set[UOM] = values.keySet.toSet
  def percentageDifference(other:PivotQuantity) = {
    if (
        errors.isEmpty && other.errors.isEmpty &&
        values.size == 1 && other.values.size == 1 &&
        values.keySet == other.values.keySet) {
      val uom = values.keySet.toList(0)
      val valueThis = values(uom)
      val valueOther = other.values(uom)
      if (valueThis == 0 || valueOther == 0) {
        if (valueThis == 0 && valueOther == 0)
          Some(Percentage(0))
        else
          None
      } else {
        val difference = valueOther-valueThis
        Some( Percentage( difference/valueThis) )
      }
    } else {
      None
    }
  }
  def *(amount:Double) : PivotQuantity = this * new Quantity(amount)
  def *(q : Quantity) : PivotQuantity = {
    new PivotQuantity(Map() ++ (for((uom,value) <- values) yield (uom * q.uom)->(value * q.value)), errors)
  }
  def <(other: PivotQuantity): Boolean = quantityValue < other.quantityValue
  def >(other: PivotQuantity): Boolean = quantityValue > other.quantityValue
  def errorState = ErrorState(hasWarning, hasErrors)
  def hasErrors = !errors.isEmpty
  def hasWarning = warning.isDefined
  def hasWarningOrErrors = hasWarning || hasErrors
  def doubleValue:Option[Double] = {
    if (hasErrors) {
      None
    } else {
      if (values.size == 1) {
        Some(values.toList(0)._2)
      } else {
        None
      }
    }
  }
  def doubleValueIgnoringErrors:Option[Double] = {
    if (values.size == 1) {
      Some(values.toList(0)._2)
    } else {
      None
    }
  }
  def quantityValue:Option[Quantity] = {
    if (hasErrors) {
      None
    } else {
      if (values.size == 1) {
        val first = values.toList(0)
        Some(Quantity(first._2, first._1))
      } else {
        None
      }
    }
  }

  def convertQuantity(f : Quantity => Quantity) : PivotQuantity = quantityValue match {
    case Some(q) => PivotQuantity(f(q))
    case None => this
  }

  def isAlmostZero = quantityValue match {
    case Some(qty) => qty.isAlmostZero
    case None => false
  }

  def explanation : Option[String] = quantityValue.map { q => q.format("#,##0.00#######################################", addSpace = true)}
}
/**
 * This type class allows the Numeric implicit enabled methods to be used, like TraversableOnce
 * product and sum methods.
 */
trait PivotQuantityIsNumeric extends Numeric[PivotQuantity] {
  def compare(p1: PivotQuantity, p2: PivotQuantity) = PivotQuantityComparator.compare(p1,p2)
  def toDouble(x: PivotQuantity) = throw new IllegalStateException("Can't convert PivotQuantity to double.")
  def toFloat(x: PivotQuantity) = throw new IllegalStateException("Can't convert PivotQuantity to float.")
  def toLong(x: PivotQuantity) = throw new IllegalStateException("Can't convert PivotQuantity to long.")
  def toInt(x: PivotQuantity) = throw new IllegalStateException("Can't convert PivotQuantity to int.")

  def fromInt(x: Int) = PivotQuantity(Quantity(x.toDouble, UOM.SCALAR))
  def negate(x: PivotQuantity) = -x
  def times(x: PivotQuantity, y: PivotQuantity) = x * y
  def minus(x: PivotQuantity, y: PivotQuantity) = x - y
  def plus(x: PivotQuantity, y: PivotQuantity) = x + y

  override def zero = PivotQuantity(Quantity(0.0, UOM.NULL))
  override def one = PivotQuantity(Quantity(1.0, UOM.SCALAR))
}

object PivotQuantity {
  implicit object PivotQuantityIsNumeric extends PivotQuantityIsNumeric
  val NULL = new PivotQuantity(Map[UOM,Double](), Map[String,List[StackTrace]]())
  def sum(qs : Seq[PivotQuantity]) = (PivotQuantity.NULL /: qs)(_+_)
  def create(values:Map[UOM,Double], errors:Set[Throwable]) = new PivotQuantity(values, errors)
  
  def calcOrCatch(fn: => Quantity): PivotQuantity = {
    try {
      PivotQuantity(fn)
    } catch {
      case e => new PivotQuantity(e)
    }
  }

  def apply(value: Quantity): PivotQuantity = value.pq
  def apply(value: Quantity, errors: Set[Throwable]): PivotQuantity = this.apply(value).copyErrors(errors)

  def throwableToError(errors: Set[Throwable]): Map[String, List[StackTrace]] =
    errors.map{StackTrace.apply}.groupBy(_.message).mapValues(_.toList)
}


object PivotQuantityComparator extends Ordering[Any] {
  def compare(a1: Any, a2: Any) = {
    (a1,a2) match {
      case (p1:PivotQuantity,p2:PivotQuantity) => {
        val p1q = p1.quantityValue
        val p2q = p2.quantityValue
        (p1q,p2q) match {
          case (Some(q1), Some(q2)) => QuantityComparator.compare(q1,q2)
          case (Some(_), None) => -1
          case (None, Some(_)) => 1
          case _ => 0
        }
      }
    }
  }
}

object QuantityComparator extends Ordering[Any] {
  def compare(a1:Any, a2:Any) = {
    (a1,a2) match {
      case (q1:Quantity,q2:Quantity) => q1.uom.compare(q2.uom) match {
        case 0 => {
          if (q1.value < q2.value)
            -1
          else if (q1.value > q2.value)
            1
          else 0
        }
        case i => i
      }
    }
  }
}

object SpreadOrQuantityComparator extends Ordering[Any] {
  def compare(a1: Any, a2: Any) = {
    (a1,a2) match {
      case (s1:SpreadOrQuantity, s2:SpreadOrQuantity) => {
        (s1.either,s2.either) match {
          case (Left(q1), Left(q2)) => QuantityComparator.compare(q1,q2)
          case (Right(sp1), Right(sp2)) => {
            QuantityComparator.compare(sp1.front, sp2.front) match {
              case 0 => QuantityComparator.compare(sp1.back, sp2.back)
              case n => n
            }
          }
          case (Left(_), Right(_)) => 1
          case (Right(_), Left(_)) => -1
        }
      }
    }
  }
}
