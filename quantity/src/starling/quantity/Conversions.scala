package starling.quantity

import UOM._
import java.io.Serializable
import starling.utils.ImplicitConversions._
import starling.utils.cache.CacheFactory

/**
 * Converts between UOMs given a table of conversions.
 */
protected[quantity] class Conversions(val conversions: Map[UOM, BigDecimal]) extends Serializable {

  val baseConverions = conversions.map {
    case (u@UOM(uType, _, _), _) => (uType -> u)
  }

  private val conversionCache = CacheFactory.getCache("Conversions.conversionCache", unique = true)

  /**
   * Don't ever call this directly. Use Quantity.in
   */
  protected[quantity] def convert(from: UOM, to: UOM): Option[BigDecimal] = conversionCache.memoize((from, to), {if (from == to) {
    Some(BigDecimal(1.0))
  } else if (to.isScalar || to.isNull) {
    Some(BigDecimal(1.0))
  } else {
    (from.div(to)) match {
      case (UOM.SCALAR, v) => {
        // e.g. USD -> US_CENT
        Some(v)
      }
      case (convert, v) if v == 1.0 => {
        val (num, numPow) = decomposePrime(convert.uType.numerator)
        val (den, denPow) = decomposePrime(convert.uType.denominator)
        val convertNoPowers = UOM.fromSymbolMap(convert.asSymbolMap.map{case (k, v) => (k -> (v/v.abs))})
        assert(numPow == denPow, "Trying to convert with different powers: " + (from, to))
        val lookup = Ratio(num, den)
        baseConverions.get(lookup) match {
          case Some(u) => {
            val (SCALAR, c1) = u.div(convertNoPowers)
            val c2 = conversions(u)
            val conversion = BigDecimal("1") / (c2 * c1)
            Some(conversion.pow(numPow))
          }
          case _ => baseConverions.get(lookup.inverse) match {
            case Some(u) => {
              val (SCALAR, c1) = u.mult(convertNoPowers)
              val c2 = conversions(u)
              val conversion = c2 * c1
              Some(conversion.pow(numPow))
            }
            case None => None
          }
        }
      }
    }
  }})

  private def decomposePrime(long: Long): (Int, Int) = {
    UOM.decomposePrimes(long).toList match {
      case (p, n) :: Nil => (p, n)
    }
  }

  def +(conv: (UOM, BigDecimal)): Conversions = new Conversions(conversions + conv)

  override def toString = conversions.map(_.toString).mkString(", ")

  override def equals(obj: Any) = obj match {
    case other: Conversions => this.conversions == other.conversions
    case _ => false
  }

  override def hashCode = conversions.hashCode
}

object Conversions {

  /**
   * Creates a new set of conversions built on top of the fixed conversions.
   */
  def apply(extra: Map[UOM, BigDecimal]) = {
    new Conversions(default.conversions ++ extra)
  }

  val default = new Conversions(Map.empty)
}