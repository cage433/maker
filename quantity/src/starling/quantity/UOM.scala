package starling.quantity

import starling.utils.CaseInsensitive._
import starling.utils.cache.CacheFactory

import starling.utils.ImplicitConversions._
import starling.utils.Pattern._
import collection.immutable.Map
import starling.utils.{StarlingEnum, CaseInsensitive}


object UOM extends StarlingEnum(classOf[UOM], (u: UOM) => u.toString, ignoreCase = true) {
  def apply(symbol: UOMSymbol): UOM = new UOM(Ratio(UOMType.unique.prime, 1), Ratio(symbol.prime, 1), 1.0)

  def apply(uomType: UOMType, symbol: UOMSymbol, scale: BigDecimal): UOM = new UOM(Ratio(uomType.prime, 1), Ratio(symbol.prime, 1), scale)

  override val Parse: Extractor[Any, UOM] = Extractor.from[Any]((a: Any) => fromStringOption(a.toString))
  val Currency: Extractor[Any, UOM] = Parse.filter(_.isCurrency)

  import UOMSymbol._
  import UOMType.Currencies

  val NULL = new UOM(Ratio(0, 1), Ratio(0, 1), 1.0)
  val SCALAR = new UOM(Ratio(1, 1), Ratio(1, 1), 1.0)
  val PERCENT = new UOM(Ratio(1, 1), Ratio(PERCENT_SYMBOL.prime, 1), .01)

  val AED = UOM(Currencies.AED, aed, 1.0)
  val AUD = UOM(Currencies.AUD, aud, 1.0)
  val BGN = UOM(Currencies.BGN, bgn, 1.0)
  val BRL = UOM(Currencies.BRL, brl, 1.0)
  val CAD = UOM(Currencies.CAD, cad, 1.0)
  val CHF = UOM(Currencies.CHF, chf, 1.0)
  val CNY = UOM(Currencies.CNY, cny, 1.0)
  val CZK = UOM(Currencies.CZK, czk, 1.0)
  val DKK = UOM(Currencies.DKK, dkk, 1.0)
  val EUR = UOM(Currencies.EUR, eur, 1.0)
  val GBP = UOM(Currencies.GBP, gbp, 1.0)
  val HKD = UOM(Currencies.HKD, hkd, 1.0)
  val HRK = UOM(Currencies.HRK, hrk, 1.0)
  val HUF = UOM(Currencies.HUF, huf, 1.0)
  val IDR = UOM(Currencies.IDR, idr, 1.0)
  val ILS = UOM(Currencies.ILS, ils, 1.0)
  val INR = UOM(Currencies.INR, inr, 1.0)
  val JPY = UOM(Currencies.JPY, jpy, 1.0)
  val KRW = UOM(Currencies.KRW, krw, 1.0)
  val LTL = UOM(Currencies.LTL, ltl, 1.0)
  val LVL = UOM(Currencies.LVL, lvl, 1.0)
  val MXN = UOM(Currencies.MXN, mxn, 1.0)
  val MYR = UOM(Currencies.MYR, myr, 1.0)
  val NAD = UOM(Currencies.NAD, nad, 1.0)
  val NOK = UOM(Currencies.NOK, nok, 1.0)
  val NZD = UOM(Currencies.NZD, nzd, 1.0)
  val PHP = UOM(Currencies.PHP, php, 1.0)
  val PLN = UOM(Currencies.PLN, pln, 1.0)
  val RON = UOM(Currencies.RON, ron, 1.0)
  val RUB = UOM(Currencies.RUB, rub, 1.0)
  val SEK = UOM(Currencies.SEK, sek, 1.0)
  val SGD = UOM(Currencies.SGD, sgd, 1.0)
  val THB = UOM(Currencies.THB, thb, 1.0)
  val TRY = UOM(Currencies.TRY, trySymbol, 1.0)
  val US_CENT = UOM(Currencies.USD, US_CENT_SYMBOL, .01)
  val USD = UOM(Currencies.USD, usd, 1.0)
  val WSC = UOM(Currencies.WSC, WSC_SYMBOL, 1.0)
  val ZAR = UOM(Currencies.ZAR, zar, 1.0)

  // MASS
  val G = UOM(UOMType.MASS, GRAM_SYMBOL, 1.0)
  val KG = UOM(UOMType.MASS, KILOGRAM_SYMBOL, 1000.0)
  val MT = UOM(UOMType.MASS, TONNE_SYMBOL, 1e6)
  val ST = UOM(UOMType.MASS, SHORT_TONNE_SYMBOL, 907184.74)
  val C_MT = UOM(UOMType.MASS, C_TONNE_SYMBOL, 100 * 1e6)
  val K_MT = UOM(UOMType.MASS, KILO_TONNE_SYMBOL, 1000 * 1e6)
  val LB = UOM(UOMType.MASS, POUND_SYMBOL,.45359237 * KG.v)
  val OZ = UOM(UOMType.MASS, OUNCE_SYMBOL, LB.v * .06857) // Troy OZ

  // Volume
  val ML = UOM(UOMType.VOLUME, MILLILITRE_SYMBOL, 1.0)
  val L = UOM(UOMType.VOLUME, LITRE_SYMBOL, 1000)
  val KL = UOM(UOMType.VOLUME, KILOLITRE_SYMBOL, 1000 * 1000)
  val M3 = UOM(UOMType.VOLUME, CUBIC_METRE_SYMBOL, 1e6)
  val C_M3 = UOM(UOMType.VOLUME, C_CUBIC_METRE_SYMBOL, 100 * 1e6)

  // Oil Volume
  val GAL = UOM(UOMType.OIL_VOLUME, GALLON_SYMBOL, 1.0)
  val K_GAL = UOM(UOMType.OIL_VOLUME, KILGALLON_SYMBOL, GAL.v * 1000)
  val BBL = UOM(UOMType.OIL_VOLUME, BARREL_SYMBOL, GAL.v * 42)
  val K_BBL = UOM(UOMType.OIL_VOLUME, KILOBARREL_SYMBOL, BBL.v * 1000)

  // Time
  val MILLISECONDS = UOM(UOMType.TIME, MILLISECONDS_SYMBOL, 1.0)

  // Heat energy
  val THERMS = UOM(UOMType.HEAT_ENERGY, THERMS_SYMBOL, 1)
  val MMBTU = UOM(UOMType.HEAT_ENERGY, MMBTU_SYMBOL, 10)

  // Other
  val SHARE = UOM(SHARE_SYMBOL)

  val DAY = UOM(DAY_SYMBOL)
  val MONTH = UOM(MONTH_SYMBOL)
  val YEAR = UOM(YEAR_SYMBOL)

  val COMEX_GOLD_LOTS = UOM(COMEX_GOLD_LOTS_SYMBOL)
  val LME_LEAD_LOTS = UOM(LME_LEAD_LOTS_SYMBOL)
  val COMEX_SILVER_LOTS = UOM(COMEX_SILVER_LOTS_SYMBOL)
  val LME_ALUMINIUM_LOTS = UOM(LME_ALUMINIUM_LOTS_SYMBOL)
  val LME_COPPER_LOTS = UOM(LME_COPPER_LOTS_SYMBOL)
  val LME_NICKEL_LOTS = UOM(LME_NICKEL_LOTS_SYMBOL)
  val SHANGHAI_RUBBER_LOTS = UOM(SHANGHAI_RUBBER_LOTS_SYMBOL)
  val LME_ZINC_LOTS = UOM(LME_ZINC_LOTS_SYMBOL)
  val LME_TIN_LOTS = UOM(LME_TIN_LOTS_SYMBOL)
  val LME_MOLYBDENUM_LOTS = UOM(LME_MOLYBDENUM_LOTS_SYMBOL)
  val COMEX_PALLADIUM_LOTS = UOM(COMEX_PALLADIUM_LOTS_SYMBOL)
  val STEEL_REBAR_SHANGHAI_LOTS = UOM(STEEL_REBAR_SHANGHAI_LOTS_SYMBOL)
  val IRON_ORE_LOTS = UOM(IRON_ORE_LOTS_SYMBOL)
  val COMEX_PLATINUM_LOTS = UOM(COMEX_PLATINUM_LOTS_SYMBOL)
  val BALTIC_PANAMAX_LOTS = UOM(BALTIC_PANAMAX_LOTS_SYMBOL)
  val NYMEX_WTI_LOTS = UOM(NYMEX_WTI_LOTS_SYMBOL)
  val NYMEX_GASOLINE_LOTS = UOM(NYMEX_GASOLINE_LOTS_SYMBOL)
  val ICE_BRENT_LOTS = UOM(ICE_BRENT_LOTS_SYMBOL)
  val ICE_GAS_OIL_LOTS = UOM(ICE_GAS_OIL_LOTS_SYMBOL)
  val NYMEX_HEATING_LOTS = UOM(NYMEX_HEATING_LOTS_SYMBOL)
  val NYMEX_SINGAPORE_FUEL_OIL_LOTS = UOM(NYMEX_SINGAPORE_FUEL_OIL_LOTS_SYMBOL)
  val NYMEX_NATGAS_LOTS_LOTS = UOM(NYMEX_NATGAS_LOTS_SYMBOL)
  val DUBAI_CRUDE_LOTS = UOM(DUBAI_CRUDE_LOTS_SYMBOL)

  val BUSHEL_SOY = UOM(BUSHEL_SOY_SYMBOL)
  val BUSHEL_CORN = UOM(BUSHEL_CORN_SYMBOL)
  val BUSHEL_WHEAT = UOM(BUSHEL_WHEAT_SYMBOL)

  val uoms = values filterNot List(NULL, SCALAR).contains
  val primes = uoms.flatMap{case UOM(Ratio(p1, _), Ratio(p2, _), _) => Set(p1.toInt, p2.toInt)}.distinct.filterNot(_ == 1)

  lazy val currencies: List[UOM] = uoms.filter(_.isCurrency)
  val uomMap: Map[(Long, Long), UOM] = uoms.map(u => (u.uType.numerator, u.subType.numerator) -> u).toMap
  val symbolToUOMMap: Map[UOMSymbol, UOM] = uoms.map(u => (UOMSymbol.symbolForPrime(u.subType.numerator) -> u)).toMap

  /**
   * Map of UOM symbol to their base - e.g. CENT -> USD or KG -> G
   */
  protected[quantity] val baseMap: Map[UOMSymbol, UOMSymbol] = {
    val bases: Map[Ratio, UOMSymbol] = uoms.flatMap {
      case UOM(uType, subType, v) if v == 1.0 => Some((uType -> UOMSymbol.symbolForPrime(subType.numerator)))
      case _ => None
    }.toMap
    uoms.flatMap {
      case UOM(uType, subType, _) => bases.get(uType).map(b => (UOMSymbol.symbolForPrime(subType.numerator) -> b))
    }.toMap + (PERCENT_SYMBOL -> PERCENT_SYMBOL)
  }

  private val baseConversionCache = CacheFactory.getCache("UOM.baseConversionCache", unique = true)

  protected[quantity] def baseFor(uom: UOM): UOM = baseConversionCache.memoize(uom, {
    val base = uom.asSymbolMap.map {
      case (sym, pow) => (UOM.baseMap(sym) -> pow)
    }
    UOM.fromSymbolMap(base)
  })

  private def getSymbolOption(text: CaseInsensitive): Option[UOM] = UOMSymbol.fromName(text).map(UOM.asUOM)

  private var uomCache = CacheFactory.getCache("UOM.fromString", unique = true)
  private val FXRegex = """(.*)([ ]?per[ ]?|/)(.*)""".r

  def fromStringOption(text: String): Option[UOM] = {
    uomCache.memoize((text), (tuple: (String)) => {
      text match {
        // check the length so that S/T doesn't fall in here. I don't know any FX that is 3 chars or less
        case FXRegex(num, _, dem) if text.length > 3 => (getSymbolOption(num.trim), getSymbolOption(dem.trim)) partialMatch {
          case (Some(n), Some(d)) => n.div(d)._1
        }
        case _ => getSymbolOption(text.trim)
      }
    })
  }

  def fromString(uomString: String): UOM = {
    fromStringOption(uomString) match {
      case None => throw new IllegalStateException("Don't recognise symbol named '" + uomString + "'")
      case Some(sym) => sym
    }
  }

  def parseCurrency(text: String): UOM = {
    val u = fromString(text)
    require(u.isCurrency, u + " is not a currency")
    u
  }

  def fromIdentifier(uomString: String) = fromString(uomString)

  def fromSymbolMap(symbolMap: Map[UOMSymbol, Int], empty: UOM = NULL): UOM = {
    if (symbolMap.isEmpty) {
      empty
    } else {
      (UOM.SCALAR /: symbolMap.toList) {
        case (accumulator, (sym, power)) => {
          val uom = symbolToUOMMap(sym)
          accumulator.mult((uom ^ power))._1
        }
      }
    }
  }

  def asUOM(uomSymbol: UOMSymbol): UOM = symbolToUOMMap(uomSymbol)

  def decomposePrimes(n: Long): Map[Int, Int] = {
    def recurse(n: Long, primes: List[Int], acc: Map[Int, Int]): Map[Int, Int] = n match {
      case 0 => acc // Should only happen for the null unit
      case 1 => acc
      case _ => primes match {
        case p :: rest =>
          if (n % p == 0)
            recurse(n / p, primes, acc + (p -> (acc.getOrElse(p, 0) + 1)))
          else
            recurse(n, rest, acc)
        case Nil =>
          throw new IllegalStateException("Prime decomposition is badly wrong: " + n)
      }
    }
    recurse(n, UOM.primes, Map.empty[Int, Int])
  }
}

/**
 * uType - main unit type such as USD or Mass or Volume. Main types cannot be converted between each other
 * without an additional converter.
 *
 * subType - sub type such as Cents or MT or L. Subtypes of the same main type are automattically converted
 * between each other.
 * 
 * v - the multiple of the base unit -- if 1.0 this is the base unit -- so it's
 *     important to put currencies in as:
 *     USD - 1.0
 *     Cent - .01
 *     etc..
 */
case class UOM(uType: Ratio, subType: Ratio, v: BigDecimal) extends Ordered[UOM] {

  def plus(o: UOM): Option[BigDecimal] = o match {
    case UOM(`uType`, `subType`, _) => Some(1.0)
    case UOM(`uType`, oSubType, oV) => Some(oV / v)
    case _ => None
  }

  def minus(o: UOM): Option[BigDecimal] = this.plus(o)

  def /(o: UOM) = this.div(o) match {
    case (u, bd) if bd == 1 => u
    case (u, bd) => throw new Exception("Can't ignore result of division " + this + "/" + o + ": " +(u, bd))
  }

  def *(o: UOM) = this.mult(o) match {
    case (u, bd) if bd == 1 => u
    case (u, bd) => throw new Exception("Can't ignore result of mult " + this + "*" + o + ": " +(u, bd))
  }

  def div(o: UOM) = this.mult(o.inverse)

  def mult(o: UOM): (UOM, BigDecimal) = {
    (this, o) match {
      case (UOM(Ratio(1, 1), _, c), _) => (o, c)
      case (_, UOM(Ratio(1, 1), _, c)) => (this, c)
      case _ => {
        val notReduced = UOM(uType * o.uType, subType * o.subType, v * o.v)
        val uGCD = notReduced.uType.gcd
        if (uGCD > 1) {
          val sGCD = notReduced.subType.gcd
          val decomposedU = UOM.decomposePrimes(uGCD)
          val decomposedS = UOM.decomposePrimes(sGCD)
          if (decomposedU.values.sum == decomposedS.values.sum) {
            val reduced = notReduced.copy(uType = notReduced.uType.reduce, subType = notReduced.subType.reduce)

            (reduced, 1.0)
          } else {
            def remove(pp: Long, primes: List[Long], removed: Int, max: Int): Long = if (removed < max) {
              val prime = primes.find(p => pp % p == 0).get
              val newPP = pp / prime
              remove(newPP, primes, removed + 1, max)
            } else {
              pp
            }
            var subType = notReduced.subType
            decomposedU.map {
              case (u, num) => {
                val matches = UOM.uomMap.keySet.filter {
                  case (a, _) => a == u
                }.map(_._2).toList
                subType = Ratio(remove(subType.numerator, matches, 0, num), remove(subType.denominator, matches, 0, num))
              }
            }

            val uType = notReduced.uType.reduce
            val reducedWrongV = notReduced.copy(uType, subType)
            val reduced = UOM.fromSymbolMap(reducedWrongV.asSymbolMap, UOM.SCALAR)
            val multiplier = (reducedWrongV.v / reduced.v)
            (reduced, multiplier)
          }
        } else {
          (notReduced, 1.0)
        }
      }
    }
  }

  def ^(power: Int) = {
    val absPower = (this /: (2 to power.abs)) {
      case (a, _) => a * this
    }
    if (power < 0) {
      absPower.inverse
    } else {
      absPower
    }
  }

  def inverse = UOM(uType.inverse, subType.inverse, 1 / v)

  def isNull = this == UOM.NULL

  def isScalar = this == UOM.SCALAR
  def isPercent = this == UOM.PERCENT

  override def toString = asString

  def compare(rhs: UOM) = asString.compareTo(rhs.asString)

  override def hashCode() = uType.hashCode() ^ subType.hashCode()

  override def equals(obj: Any) = obj match {
    case UOM(`uType`, `subType`, _) => true
    case _ => false
  }

  def identifier = asString

  lazy val asString = {
    val symMap = asSymbolMap.map {
      case (sym, power) => (sym.name, power)
    }.toList.sortWith(_._1 < _._1)

    val num = symMap.map {
      case (symName, power) if power == 1 => symName
      case (symName, power) if power > 1 => symName + "^" + power
      case _ => ""
    }.foldLeft("")(_ + _)

    val den = symMap.map {
      case (symName, power) if power == -1 => symName
      case (symName, power) if power < -1 => symName + "^" + -power
      case _ => ""
    }.foldLeft("")(_ + _)

    val sep = if (isFX) " per " else "/"

    (num, den) match {
      case (num, "") => num
      case ("", den) if den.contains('^') => den.replace("^", "^-")
      case ("", den) => den + "^-1"
      case _ => num + sep + den
    }
  }

  def asSymbolMap: Map[UOMSymbol, Int] = {
    // Note - both NULL and SCALAR become an empty map

    val reducedUOM = this
    val negativePowers = UOM.decomposePrimes(reducedUOM.subType.denominator).mapValues(_ * -1)
    val primePowers = UOM.decomposePrimes(reducedUOM.subType.numerator) ++ negativePowers
    Map.empty ++ primePowers.map {
      case (p, n) => (UOMSymbol.symbolForPrime(p) -> n)
    }
  }

  def numeratorUOM: UOM = this match {
    case UOM.NULL => UOM.NULL
    case _ => UOM.fromSymbolMap(asSymbolMap.filterValues(_ > 0), UOM.SCALAR)
  }

  def denominatorUOM: UOM = this match {
    case UOM.NULL => UOM.SCALAR // matching logic from before refactor
    case _ => {
      val filterValues = asSymbolMap.filterValues(_ < 0)
      val uom = UOM.fromSymbolMap(filterValues, UOM.SCALAR)
      uom.inverse
    }
  }

  def replace(uom1: UOM, uom2: UOM) = {
    def recurse(u: UOM): UOM = {
      if (u.uType.gcd(uom1.uType) == uom1.uType)
        uom2 * recurse(u / uom1)
      else if (u.uType.gcd(uom1.uType.inverse) == uom1.uType.inverse)
        recurse(u * uom1) / uom2
      else
        u
    }
    if (uom1 == uom2)
      this
    else
      recurse(this)
  }

  def isCurrency: Boolean = UOMType.Currencies.isCurrency(uType)

  /**
   * E.g. if this is US_CENT will return USD, or Pence will return GBP
   */
  def inBaseCurrency: UOM = {
    assert(isCurrency, "Not a currency: " + this)
    inBaseUnit
  }

  def isBaseCurrency: Boolean = {
    isCurrency && v == BigDecimal("1.0")
  }

  def isBaseUnit: Boolean = {
    v == BigDecimal("1.0")
  }


  def inBaseUnit: UOM = if (isBaseUnit) {
    this
  } else {
    UOM.baseFor(this)
  }

  def isFX = numeratorUOM.isCurrency && denominatorUOM.isCurrency
}
