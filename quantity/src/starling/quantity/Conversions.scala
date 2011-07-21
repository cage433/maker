package starling.quantity

import UOM._
import java.io.Serializable

/**
 * Converts between UOMs given a table of conversions.
 */
class Conversions(val conversions: Map[UOM, Double]) extends Serializable {
  // conversion rate from one UOM to another, returning a ratio if a fixed conversion can be found,
  // or None if it can't.
  def convert(from: UOM, to: UOM): Option[Double] = {
    val commonUOM = from gcd to
    val (distinctFrom, distinctTo) = (from / commonUOM, to / commonUOM)

    conversionRate(distinctFrom.unscaled, distinctTo.unscaled).
      map(rate => (distinctFrom / distinctTo).scale.doubleValue * rate).
      //orElse(allConversionsFrom(distinctFrom, Set(distinctFrom)).get(distinctTo)).
      orElse(convertBySymbol(distinctFrom, distinctTo))
  }

  private def conversionRate(from: UOM, to: UOM): Option[Double] = {
    conversions.get(to / from).
      orElse(conversions.get(from / to).map(1.0 / _))
  }

  // returns a map of all conversions available from the given argument UOM to the multiplicative
  // factor needed to convert them.
  private def conversionsFrom(uom: UOM): Map[UOM, Double] = {
    val simpleUnits = (Set.empty[UOM] /: conversions.keysIterator)((s, x) => {
      s + x.numeratorUOM + x.denominatorUOM
    })
    (Map.empty[UOM, Double] /: simpleUnits)((m, x) => {
      conversionRate(uom, x).map((r) => {
        m + (x -> r)
      }).getOrElse(m)
    })
  }

  // recursively finds all conversions from a single UOM until it has visited all UOMs convertible
  // from the first given argument.
  private def allConversionsFrom(uom: UOM, seen: Set[UOM]): Map[UOM, Double] = {
    val nextSteps = (Map.empty[UOM, Double] /: conversionsFrom(uom).filterKeys((u) => !seen.contains(u)))(_ + _)
    val nowSeen = seen ++ nextSteps.keysIterator
    (nextSteps /: nextSteps.keysIterator)((m, k) => {
      m ++ allConversionsFrom(k, nowSeen).mapValues(_ * nextSteps(k))
    })
  }

  // try and convert each pair of symbols after splitting the UOM
  private def convertBySymbol(from: UOM, to: UOM): Option[Double] = {
    if (from.isScalar && to.isScalar) {
      Some((to / from).scale.doubleValue)
    } else {
      val powerMap: Map[UOM, Int] = from.asSymbolMap.map(tuple => (tuple._1.asUOM, tuple._2))
      val maybeFirstFromUOM = powerMap.headOption.map(_._1)

      maybeFirstFromUOM.flatMap(firstFrom => {
        val allConversions = allConversionsFrom(firstFrom, Set(firstFrom))
        val possibleToUOMs = to.asSymbolMap.keySet.map(_.asUOM) intersect allConversions.keySet
        val power = powerMap(firstFrom)
        val reducedFrom = from / (firstFrom ^ power)

        possibleToUOMs.foldLeft[Option[Double]](None)((maybe, uom) => maybe.orElse({
          val reducedTo = to / (uom ^ power)
          val rate = convertBySymbol(reducedFrom, reducedTo)
          rate.map(_ * scala.math.pow(allConversions(uom), power))
        }))
      })
    }
  }

  def + (conv: (UOM, Double)): Conversions = new Conversions(conversions + conv)

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
  def apply(extra: Map[UOM, Double]) = {
    new Conversions(default.conversions ++ extra)
  }

  trait UnitType {
    val base: UOM
  }

  case object Mass extends UnitType {
    val base = G
  }

  case object Volume extends UnitType {
    val base = ML
  }

  private val types = Map(
    MT -> Mass,
    K_MT -> Mass,
    C_MT -> Mass,
    LB -> Mass,
    G -> Mass,
    KG -> Mass,
    OZ -> Mass,

    M3 -> Volume,
    C_M3 -> Volume,
    KL -> Volume,
    L -> Volume,
    ML -> Volume,
    C_M3 -> Volume,
    GAL -> Volume,
    BBL -> Volume,
    K_BBL -> Volume
  )

  /**
   * fixed conversions between units. taken from http://www.unc.edu/~rowlett/units/index.html
   *
   * don't put market-dependent conversions here (e.g: MT/BBL), instead create a local instance
   * of the conversions class with the additional rate.
   */
  val fixed_conversions: Map[UOM, Double] = Map(
    US_CENT / USD -> 100,
    G / MT -> 1000000,
    G / OZ -> 31.1034768,
    G / LB -> 453.59237,
    MT / LB -> 1000000 / 453.59237,
    MT / K_MT -> 1000,
    L / KL -> 1000,
    ML / L -> 1000,
    L / GAL -> 3.785411784,
    L / M3 -> 1000,
    GAL / BBL -> 42.0,
    K_BBL / BBL -> 1e-3,
    C_MT / MT -> 1e-2,
    C_M3 / M3 -> 1e-2
  )

  // default "safe" conversions which can be used in any circumstances and aren't tied to a particular
  // market.
  val default = new Conversions(fixed_conversions)

  /**
   * Returns the base UOM for a Unit. For example the base unit of MT is G.
   */
  def baseUOM(uom: UOM): Option[UOM] = types.get(uom).map(_.base)
}