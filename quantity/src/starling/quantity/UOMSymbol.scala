package starling.quantity

import starling.utils.CaseInsensitive
import starling.utils.CaseInsensitive._

object UOMSymbol{
  def apply(name : String, altName: String) : UOMSymbol = {
    UOMSymbol(name, List(altName))
  }
  def apply(name : String, altNames: List[String]) : UOMSymbol = {
    new UOMSymbol(name, altNames.map(CaseInsensitive(_)))
  }
  def apply(names: List[String]): UOMSymbol = apply(names.head, names.tail)

  val SHARE_SYMBOL = UOMSymbol("Share")

  // The compiler rejects assigning all the currencies in one go, so have to create 3 separate lists
  val zCurrencies@List(aed, aud, bgn, brl, cad, chf, cny, czk, dkk, eur, gbp, hkd, hrk, huf, idr) =
    currencies("AED", "AUD", "BGN", "BRL", "CAD", "CHF", "CNY/RMB", "CZK", "DKK", "EUR", "GBP", "HKD", "HRK", "HUF", "IDR")

  val zCurrencies2@List(ils, inr, jpy, krw, ltl, lvl, mxn, myr, nad, nok, nzd, php, pln, ron, rub, sek) =
    currencies("ILS", "INR", "JPY", "KRW", "LTL", "LVL", "MXN", "MYR", "NAD", "NOK", "NZD", "PHP", "PLN", "RON", "RUB", "SEK")

  val zCurrencies3@List(sgd, thb, trySymbol, usd, zar) = currencies("SGD", "THB", "TRY", "USD", "ZAR")

  private def currencies(symbols: String*): List[UOMSymbol] = symbols.toList.map(symbol => UOMSymbol(symbol.split("/").toList))

  // this isn't a currency, but a kind of index for shipping. it's used in place of a currency in
  // the EAI tables, though.
  val WSC_SYMBOL = UOMSymbol("WSC", "Worldscale")

  val TONNE_SYMBOL = UOMSymbol("MT", "TONNE")
  val C_TONNE_SYMBOL = UOMSymbol("c MT", "c TONNE")
  val KILO_TONNE_SYMBOL = UOMSymbol("KT", "KILOTONNE")
  val BARREL_SYMBOL = UOMSymbol("bbl", List("BARREL", "BBLS"))
  val KILO_BARREL_SYMBOL = UOMSymbol("K bbl", List("KILO BARREL", "kb"))
  val OUNCE_SYMBOL = UOMSymbol("oz", "TROY OUNCE")
  val POUND_SYMBOL = UOMSymbol("lb", "POUND")
  val GRAM_SYMBOL = UOMSymbol("g", "GRAM")
  val GALLON_SYMBOL = UOMSymbol("gal", "US GALLON")
  val KILOLITRE_SYMBOL = UOMSymbol("kl", "KILOLITRE")
  val LITRE_SYMBOL = UOMSymbol("l", "LITRE")
  val CUBIC_METRE_SYMBOL = UOMSymbol("m3", "CUBIC METRE")
  val C_CUBIC_METRE_SYMBOL = UOMSymbol("c m3", "C CUBIC METRE")
  val MMBTU_SYMBOL = UOMSymbol("mmbtu", "MMBTU")
  val MILLILITRE_SYMBOL = UOMSymbol("ml", "MILLILITRE")
  val THERMS_SYMBOL = UOMSymbol("thm", "THERM")
  val BUSHEL_SOY_SYMBOL = UOMSymbol("bu(soy)", "BUSHEL (SOY)")
  val BUSHEL_CORN_SYMBOL = UOMSymbol("bu(corn)", "BUSHEL (CORN)")
  val BUSHEL_WHEAT_SYMBOL = UOMSymbol("bus", "BUSHEL (WHEAT)")
  val SHORT_TON_SYMBOL = UOMSymbol("st", "SHORT TON")

  val PERCENT_SYMBOL = UOMSymbol("%")
  
  val US_CENT_SYMBOL = UOMSymbol("¢", "US CENT")

  val DAY_SYMBOL = UOMSymbol("DAY", "days")
  val MONTH_SYMBOL = UOMSymbol("MONTH")
  val YEAR_SYMBOL = UOMSymbol("YEAR")

  val COMEX_GOLD_LOTS_SYMBOL = UOMSymbol("Comex Gold Lots")
  val LME_LEAD_LOTS_SYMBOL = UOMSymbol("LME Lead Lots")
  val COMEX_SILVER_LOTS_SYMBOL = UOMSymbol("Comex Silver Lots")
  val LME_ALUMINIUM_LOTS_SYMBOL = UOMSymbol("LME Aluminium Lots")
  val LME_COPPER_LOTS_SYMBOL = UOMSymbol("LME Copper Lots")
  val LME_NICKEL_LOTS_SYMBOL = UOMSymbol("LME Nickel Lots")
  val SHANGHAI_RUBBER_LOTS_SYMBOL = UOMSymbol("Shanghai Natural Rubber Lots")
  val LME_ZINC_LOTS_SYMBOL = UOMSymbol("LME Zinc Lots")
  val LME_TIN_LOTS_SYMBOL = UOMSymbol("LME Tin Lots")
  val LME_MOLYBDENUM_LOTS_SYMBOL = UOMSymbol("LME Molybdenum Lots")
  val COMEX_PALLADIUM_LOTS_SYMBOL = UOMSymbol("Comex Palladium Lots")
  val STEEL_REBAR_SHANGHAI_LOTS_SYMBOL = UOMSymbol("Shanghai Steel Lots")
  val IRON_ORE_LOTS_SYMBOL = UOMSymbol("Iron Ore Lots")
  val COMEX_PLATINUM_LOTS_SYMBOL = UOMSymbol("Comex Platinum Lots")
  val BALTIC_PANAMAX_LOTS_SYMBOL = UOMSymbol("Baltic Panamax Lots")
  val NYMEX_WTI_LOTS_SYMBOL = UOMSymbol("Nymex WTI Lots")
  val NYMEX_GASOLINE_LOTS_SYMBOL = UOMSymbol("Nymex Gasoline Lots")
  val ICE_BRENT_LOTS_SYMBOL = UOMSymbol("ICE Brent Lots")
  val ICE_GAS_OIL_LOTS_SYMBOL = UOMSymbol("ICE Gasoil Lots")
  val NYMEX_HEATING_LOTS_SYMBOL = UOMSymbol("Nymex Heating Lots")
  val NYMEX_SINGAPORE_FUEL_OIL_LOTS_SYMBOL = UOMSymbol("Nymex Fueloil Lots")
  val NYMEX_NATGAS_LOTS_SYMBOL = UOMSymbol("Nymex NatGas Lots")
  val DUBAI_CRUDE_LOTS_SYMBOL = UOMSymbol("Dubai Crude Lots")

  val MILLISECONDS_SYMBOL = UOMSymbol("ms")

  val currencySymbols = zCurrencies ++ zCurrencies2 ++ zCurrencies3 ++ List(US_CENT_SYMBOL)

  val nonCurrencySymbols = List(
    TONNE_SYMBOL,
    C_TONNE_SYMBOL,
    KILO_TONNE_SYMBOL,
    BARREL_SYMBOL,
    KILO_BARREL_SYMBOL,
    OUNCE_SYMBOL,
    POUND_SYMBOL, 
    GRAM_SYMBOL,
    GALLON_SYMBOL,
    KILOLITRE_SYMBOL,
    LITRE_SYMBOL,
    CUBIC_METRE_SYMBOL,
    C_CUBIC_METRE_SYMBOL,
    MMBTU_SYMBOL,
    WSC_SYMBOL,
    MILLILITRE_SYMBOL,
    THERMS_SYMBOL,
    DAY_SYMBOL,
    MONTH_SYMBOL,
    YEAR_SYMBOL,
    COMEX_GOLD_LOTS_SYMBOL,
    LME_LEAD_LOTS_SYMBOL,
    LME_MOLYBDENUM_LOTS_SYMBOL,
    COMEX_SILVER_LOTS_SYMBOL,
    LME_ALUMINIUM_LOTS_SYMBOL,
    LME_COPPER_LOTS_SYMBOL,
    LME_NICKEL_LOTS_SYMBOL,
    SHANGHAI_RUBBER_LOTS_SYMBOL,
    LME_ZINC_LOTS_SYMBOL,
    LME_TIN_LOTS_SYMBOL,
    COMEX_PALLADIUM_LOTS_SYMBOL,
    STEEL_REBAR_SHANGHAI_LOTS_SYMBOL,
    IRON_ORE_LOTS_SYMBOL,
    COMEX_PLATINUM_LOTS_SYMBOL,
    BALTIC_PANAMAX_LOTS_SYMBOL,
    NYMEX_WTI_LOTS_SYMBOL,
    NYMEX_GASOLINE_LOTS_SYMBOL,
    ICE_BRENT_LOTS_SYMBOL,
    ICE_GAS_OIL_LOTS_SYMBOL,
    NYMEX_HEATING_LOTS_SYMBOL,
    NYMEX_SINGAPORE_FUEL_OIL_LOTS_SYMBOL,
    NYMEX_NATGAS_LOTS_SYMBOL,
    DUBAI_CRUDE_LOTS_SYMBOL,
    BUSHEL_SOY_SYMBOL,
    BUSHEL_CORN_SYMBOL,
    BUSHEL_WHEAT_SYMBOL,
    SHORT_TON_SYMBOL,
    MILLISECONDS_SYMBOL,
    PERCENT_SYMBOL,
    SHARE_SYMBOL
  )
  
  val symbols = currencySymbols ++ nonCurrencySymbols
  // Map of name and altname to symbol
  val symbolMap : Map[CaseInsensitive, UOMSymbol] = Map[CaseInsensitive, UOMSymbol]() ++ symbols.flatMap(sym => sym.names.map((_,sym)))
  
  // We associate a prime number with each symbol. This allows an efficient representation of
  // quantities with fast arithmetic
  val primeForSymbol : Map[UOMSymbol, Int] = Map.empty ++ symbols.zip(Primes.firstNPrimes(symbols.size))
  val symbolForPrice : Map[Int, UOMSymbol] = Map.empty ++ primeForSymbol.view.map{case (s, p) => p -> s}
  val primes = symbolForPrice.keySet.toList.sortWith(_<_)
}

case class UOMSymbol(name : CaseInsensitive, altNames : List[CaseInsensitive] = List()){
  import UOMSymbol._
  lazy val asUOM : UOM = UOM.build(primeForSymbol(this), 1)

  def names : List[CaseInsensitive] = name :: altNames

  override def toString = name
}