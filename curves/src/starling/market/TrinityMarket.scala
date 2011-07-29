package starling.market


object TrinityMarket {
  import Market._

  private val UNKNOWN_TRINITY_CODE = "???"
  // Map of trinity codes to Markets
  lazy val trinityCodeMap: Map[String, CommodityMarket] = Map(
    "GC" → COMEX_GOLD, "SI" → COMEX_SILVER, "XGC" → COMEX_GOLD, "XSI" → COMEX_SILVER, "XPD" → COMEX_PALLADIUM,
    "XPT" → COMEX_PLATINUM, "XHG" → COMEX_HIGH_GRADE_COPPER, "PL" → COMEX_PLATINUM, "PA" → COMEX_PALLADIUM,

    "XAA" → LME_ALUMINIUM_ALLOY, "XAL" → LME_ALUMINIUM, "XCO" → LME_COBALT, "XCU" → LME_COPPER, "XPB" → LME_LEAD,
    "XNI" → LME_NICKEL, "XNA" → LME_NASAAC, "XSN" → LME_TIN, "XZN" → LME_ZINC, "XFM" → LME_STEEL_BILLETS, UNKNOWN_TRINITY_CODE → LME_MOLYBDENUM,

    "XZS" → SHANGHAI_ZINC, "XCS" → SHANGHAI_COPPER, "XAS" → SHANGHAI_ALUMINUIUM, "SAU" → SHANGHAI_GOLD, "FRB" → STEEL_REBAR_SHANGHAI,

    "NCL" → NYMEX_WTI, "NHO" → NYMEX_HEATING, "NFO" → NYMEX_SINGAPORE_FUEL_OIL, "NNG" → NYMEX_NAT_GAS, "NHU" → NYMEX_GASOLINE,

    "ICO" → ICE_BRENT, "IGO" → ICE_GAS_OIL, "ICL" → ICE_WTI )


  lazy val marketToTrinityCode = trinityCodeMap.map(_.swap)
}