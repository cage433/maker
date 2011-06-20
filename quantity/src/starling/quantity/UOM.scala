package starling.quantity

import starling.utils.CaseInsensitive
import starling.utils.CaseInsensitive._
import starling.utils.cache.CacheFactory

import starling.utils.ImplicitConversions._
import starling.utils.Pattern._


object UOM {
  def apply(numerator : Long, denominator : Long) : UOM = UOM(Ratio(1,1), Ratio(numerator, denominator))

  val Parse = Extractor.from[String](fromStringOption)

  import UOMSymbol._
  val NULL = UOM.build(0, 1)
  val SCALAR = UOM.build(1, 1)

  val AED = aed.asUOM
  val AUD = aud.asUOM
  val BGN = bgn.asUOM
  val BRL = brl.asUOM
  val CAD = cad.asUOM
  val CHF = chf.asUOM
  val CNY = cny.asUOM
  val CZK = czk.asUOM
  val DKK = dkk.asUOM
  val EUR = eur.asUOM
  val GBP = gbp.asUOM
  val HKD = hkd.asUOM
  val HRK = hrk.asUOM
  val HUF = huf.asUOM
  val IDR = idr.asUOM
  val ILS = ils.asUOM
  val INR = inr.asUOM
  val JPY = jpy.asUOM
  val KRW = krw.asUOM
  val LTL = ltl.asUOM
  val LVL = lvl.asUOM
  val MXN = mxn.asUOM
  val MYR = myr.asUOM
  val NAD = nad.asUOM
  val NOK = nok.asUOM
  val NZD = nzd.asUOM
  val PHP = php.asUOM
  val PLN = pln.asUOM
  val RON = ron.asUOM
  val RUB = rub.asUOM
  val SEK = sek.asUOM
  val SGD = sgd.asUOM
  val THB = thb.asUOM
  val TRY = trySymbol.asUOM
  val USD = usd.asUOM
  val WSC = WSC_SYMBOL.asUOM
  val ZAR = zar.asUOM
  val US_CENT = US_CENT_SYMBOL.asUOM
  val SHARE = SHARE_SYMBOL.asUOM

  val MT = TONNE_SYMBOL.asUOM
  val C_MT = C_TONNE_SYMBOL.asUOM
  val K_MT = KILO_TONNE_SYMBOL.asUOM
  val BBL = BARREL_SYMBOL.asUOM
  val K_BBL = KILO_BARREL_SYMBOL.asUOM
  val OZ = OUNCE_SYMBOL.asUOM
  val LB = POUND_SYMBOL.asUOM
  val G = GRAM_SYMBOL.asUOM
  val KG = G * 1000
  val GAL = GALLON_SYMBOL.asUOM
  val KL = KILOLITRE_SYMBOL.asUOM
  val L = LITRE_SYMBOL.asUOM
  val M3 = CUBIC_METRE_SYMBOL.asUOM
  val C_M3 = C_CUBIC_METRE_SYMBOL.asUOM
  val MMBTU = MMBTU_SYMBOL.asUOM
  val THERMS = THERMS_SYMBOL.asUOM
  val ML = MILLILITRE_SYMBOL.asUOM

  val PERCENT = PERCENT_SYMBOL.asUOM

  val DAY = DAY_SYMBOL.asUOM
  val MONTH = MONTH_SYMBOL.asUOM
  val YEAR = YEAR_SYMBOL.asUOM

  val COMEX_GOLD_LOTS = COMEX_GOLD_LOTS_SYMBOL.asUOM
  val LME_LEAD_LOTS = LME_LEAD_LOTS_SYMBOL.asUOM
  val COMEX_SILVER_LOTS = COMEX_SILVER_LOTS_SYMBOL.asUOM
  val LME_ALUMINIUM_LOTS = LME_ALUMINIUM_LOTS_SYMBOL.asUOM
  val LME_COPPER_LOTS = LME_COPPER_LOTS_SYMBOL.asUOM
  val LME_NICKEL_LOTS = LME_NICKEL_LOTS_SYMBOL.asUOM
  val SHANGHAI_RUBBER_LOTS = SHANGHAI_RUBBER_LOTS_SYMBOL.asUOM
  val LME_ZINC_LOTS = LME_ZINC_LOTS_SYMBOL.asUOM
  val LME_TIN_LOTS = LME_TIN_LOTS_SYMBOL.asUOM
  val LME_MOLYBDENUM_LOTS = LME_MOLYBDENUM_LOTS_SYMBOL.asUOM
  val COMEX_PALLADIUM_LOTS = COMEX_PALLADIUM_LOTS_SYMBOL.asUOM
  val STEEL_REBAR_SHANGHAI_LOTS = STEEL_REBAR_SHANGHAI_LOTS_SYMBOL.asUOM
  val IRON_ORE_LOTS = IRON_ORE_LOTS_SYMBOL.asUOM
  val COMEX_PLATINUM_LOTS = COMEX_PLATINUM_LOTS_SYMBOL.asUOM
  val BALTIC_PANAMAX_LOTS = BALTIC_PANAMAX_LOTS_SYMBOL.asUOM
  val NYMEX_WTI_LOTS = NYMEX_WTI_LOTS_SYMBOL.asUOM
  val NYMEX_GASOLINE_LOTS = NYMEX_GASOLINE_LOTS_SYMBOL.asUOM
  val ICE_BRENT_LOTS = ICE_BRENT_LOTS_SYMBOL.asUOM
  val ICE_GAS_OIL_LOTS = ICE_GAS_OIL_LOTS_SYMBOL.asUOM
  val NYMEX_HEATING_LOTS = NYMEX_HEATING_LOTS_SYMBOL.asUOM
  val NYMEX_SINGAPORE_FUEL_OIL_LOTS = NYMEX_SINGAPORE_FUEL_OIL_LOTS_SYMBOL.asUOM
  val NYMEX_NATGAS_LOTS_LOTS = NYMEX_NATGAS_LOTS_SYMBOL.asUOM
  val DUBAI_CRUDE_LOTS = DUBAI_CRUDE_LOTS_SYMBOL.asUOM

  val BUSHEL_SOY = BUSHEL_SOY_SYMBOL.asUOM
  val BUSHEL_CORN = BUSHEL_CORN_SYMBOL.asUOM
  val BUSHEL_WHEAT = BUSHEL_WHEAT_SYMBOL.asUOM
  val SHORT_TON = SHORT_TON_SYMBOL.asUOM

  val MILLISECONDS = MILLISECONDS_SYMBOL.asUOM

  lazy val currencies = currencySymbols.map(_.asUOM)

  def build(numerator : Int, denominator : Int) : UOM = {
  		UOM(numerator, denominator).reduce
  }

  def getSymbolOption(uomString: CaseInsensitive): Option[UOMSymbol] = {
    require(!uomString.isEmpty, "Empty UOM")
    symbolMap.get(uomString)
  }

  def getSymbol(uomString: CaseInsensitive): UOMSymbol = {
    getSymbolOption(uomString) match {
      case None => throw new IllegalStateException("Don't recognise symbol named '" + uomString + "'")
      case Some(sym) => sym
    }
  }

  private var uomCache = CacheFactory.getCache("UOM.fromString", unique = true)
  private val FXRegex = """(.*)( per |/)(.*)""".r

  def fromStringOption(text: String): Option[UOM] = {
    uomCache.memoize((text), (tuple: (String)) => {
      val (scale, uomString) = if (text.startsWith("K ")) {
        (1000, text.substring(2))
      } else {
        (1, text)
      }

      val unScaledUOM = uomString match {
        case FXRegex(num, _, dem) => (getSymbolOption(num.trim), getSymbolOption(dem.trim)) partialMatch {
          case (Some(n), Some(d)) => n.asUOM / d.asUOM
        }
        case _ => getSymbolOption(uomString.trim).map(_.asUOM)
      }

      unScaledUOM.map(_ * scale)
    })
  }

  def fromString(uomString: String): UOM = {
    fromStringOption(uomString) match {
      case None => throw new IllegalStateException("Don't recognise symbol named '" + uomString + "'")
      case Some(sym) => sym
    }
  }

  def fromIdentifier(uomString : String) = fromString(uomString)

  private val allCurrencies = currencies.toMapWithKeys(_.toString)
  def parseCurrency(text:String) = allCurrencies.get(text.toUpperCase)

  def fromSymbolMap(symbolMap : Map[UOMSymbol, Int]) : UOM = {
    if (symbolMap.isEmpty)
      // We could equally return SCALAR here - not sure it matters
      UOM.NULL
    else {
      (UOM.SCALAR /: symbolMap.toList){
        case (accumulator, (sym, power)) =>
          accumulator * (sym.asUOM ^ power)
      }
    }
  }

}

/**
 * scale is e.g kilo, centi, mega etc
 */
case class UOM private (scale : Ratio, value : Ratio) extends RatioT[UOM] {
  import UOM._

  def compare(rhs: UOM) = asString.compareTo(rhs.asString)
  def *(rhs : Long) = UOM(scale * rhs, value)
  def *(rhs : UOM) = UOM(scale * rhs.scale, value * rhs.value)
  def /(rhs : UOM) = UOM(scale / rhs.scale, value / rhs.value)
  def ^(power : Int) = UOM(scale ^ power, value ^ power)
  def reduce : UOM = UOM(scale.reduce, value.reduce)
  def inverse : UOM = UOM(scale.inverse, value.inverse)
  def gcd(rhs : UOM) = UOM(scale.gcd(rhs.scale), value.gcd(rhs.value))
  def unscaled = UOM(Ratio(1,1), value)
  def replace(uom1 : UOM, uom2 : UOM) = {
    def recurse(u : UOM) : UOM = {
      if (u.gcd(uom1) == uom1)
        uom2 * recurse(u / uom1)
      else if (u.gcd(uom1.inverse) == uom1.inverse)
        recurse(u * uom1) / uom2
      else 
        u
    }
    if (uom1 == uom2)
      this
    else
      recurse(this)
  }

  override def toString = asString


  def identifier = asString

  lazy val asString : String = {
    // sort so that it's easy to see when 2 string units are the same
    val symMap = asSymbolMap.map{
      case (sym, power) => (sym.name, power)
    }.toList.sortWith(_._1 < _._1)

    val num = symMap.map {
      case (symName, power) if power == 1 => symName
      case (symName, power) if power > 1 => symName + "^" + power
      case _ => ""
    }.foldLeft("")(_+_)

    val den = symMap.map {
      case (symName, power) if power == -1 => symName
      case (symName, power) if power < -1 => symName + "^" + -power
      case _ => ""
    }.foldLeft("")(_+_)

    val sep = if (isFX) " per " else "/"

    (scale.reduce, den) match {
      case (Ratio(1000, 1), "") => "K " + num
      case (Ratio(1, 1), "")    => num
      case (other, "")          => other + " " + num
      case (Ratio(1000, 1), _)  => "K " + num + sep + den
      case (Ratio(1, 1), _)     => num + sep + den
      case (Ratio(1, 1000), _)  => num + sep + "K " + den
      case (other, _)           => other + " " + num + sep + den
    }
  }

  def asSymbolMap() : Map[UOMSymbol, Int] = {
    // Note - both NULL and SCALAR become an empty map
  	def recurse (n : Long, primes : List[Int], acc : Map[Int, Int]) : Map[Int, Int] = n match {
      case 0 => acc		// Should only happen for the null unit
      case 1 => acc
      case _ => primes match {
        case p :: rest =>
          if (n % p == 0)
            recurse(n / p, primes, acc + (p -> (acc.getOrElse(p, 0) + 1)))
          else
            recurse(n, rest, acc)
        case Nil =>
          throw new IllegalStateException("Prime decomposition is badly wrong")
      }
    }
    def decompose(n : Long) : Map[Int, Int] = recurse(n, UOMSymbol.primes, Map.empty[Int, Int])
    val reducedUOM = reduce
    val negativePowers = decompose(reducedUOM.value.denominator).mapValues(_ * -1)
    val primePowers = decompose(reducedUOM.value.numerator) ++ negativePowers
    Map.empty ++ primePowers.map{case (p, n) => (UOMSymbol.symbolForPrice(p) -> n)}
  }


  def numeratorUOM : UOM = UOM(scale.numeratorRatio, Ratio(reduce.value.numerator, 1))
  def denominatorUOM : UOM = UOM(scale.denominatorRatio, Ratio(reduce.value.denominator, 1))

  def isNull = this == NULL
  def isScalar = this.value == SCALAR.value
  def isCurrency = currencies.contains(this)
  def isProperUOM = !(isScalar || isNull)
  def isFX = numeratorUOM.isCurrency && denominatorUOM.isCurrency
}