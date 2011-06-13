package starling.market

import rules.{Precision, MarketPrecisionFactory}
import starling.curves._
import scala.None
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.db._
import starling.daterange._
import collection.immutable.Map
import util.matching.Regex
import java.util.{NoSuchElementException, WeakHashMap}
import starling.utils.cache.CacheFactory
import starling.varcalculator.{VaRRiskFactor, ForwardPriceRiskFactor, RiskFactor}
import starling.calendar._
import starling.quantity.{Quantity, UOM}
import starling.marketdata.MarketDataKey
import starling.utils.ImplicitConversions._

trait Market extends Ordered[Market]{
  val name: String
  val uomName: String

  def compare(that: Market) = this.name.compareTo(that.name)
}


/**
 * A Market can be thought of as something which has an associated forward curve. There may or may not
 * be fixings also associated with it. The variables mirror those in the var reporting database,
 * however not all are necessary. For example lotSize and expiryRule only apply to futures markets.
 */
abstract class CommodityMarket(
  override val name : String,
  @transient val lotSize : Option[Double],
  @transient val uom : UOM,
  @transient val currency : UOM,
  @transient val businessCalendar : BusinessCalendarSet,
  @transient val eaiQuoteID : Option[Int],
  @transient val pricesTable: ForwardCurvePricingTable,
  @transient val tenor: TenorType,
  @transient val commodity : Commodity,
  @transient val limSymbol : Option[LimSymbol] = None,
  @transient val precision : Option[Precision] = None
)
  extends Market with HasImpliedVol
{
  val uomName = uom.toString

  override def equals(p1: Any) = p1 match {
    case et: CommodityMarket => eaiQuoteID == et.eaiQuoteID && name == et.name
    case _ => super.equals(p1)
  }

  override def toString = name

  override def hashCode = name.hashCode ^ eaiQuoteID.hashCode

  def curveKey = ForwardCurveKey(this)

  def positionUOM = uom

  @transient protected lazy val observationDayCache = CacheFactory.getCache(name, unique = true)

  def observationDays(period : DateRange) : List[Day] = observationDayCache.memoize(period, period.days.filter(isObservationDay).toList)
  
  @transient val priceUOM = currency / uom

  /** Whether or not market prices observed on this day
   */
  def isObservationDay(day : Day) = businessCalendar.isBusinessDay(day)

  def priceRiskFactor(marketDayAndTime: DayAndTime, dateRange: DateRange): Option[RiskFactor]

  def nthPeriod(dayAndTime: DayAndTime, numPeriodsAhead: Int): DateRange

  def nthOptionPeriod(dayAndTime: DayAndTime, numPeriodsAhead: Int): DateRange = nthPeriod(dayAndTime, numPeriodsAhead)

  def standardShift = {
    commodity match {
      case _: OilCommodity if currency == USD => {
        // for oil a shift of 25c/BBL works well. We convert it so that GAL markets work too.
        convertUOM(Quantity(0.25, USD / BBL), priceUOM)
      }
      case _ => Quantity(0.25, priceUOM)
    }
  }

  //For markets which trade on two exchanges we only store the market data against the main exchange
  def marketDataMarket = CommodityMarket.remap.getOrElse(this, this)

  def convert(value: Quantity, uom: UOM): Option[Quantity] = {
    value.in(uom)(commodity.conversions)
  }

  def convertUOM(volume : Quantity, uom : UOM) : Quantity = {
    convert(volume, uom) match {
      case Some(beqv) => beqv
      case None => throw new Exception(this + ": Couldn't convert from " + volume + " to " + uom)
    }
  }

  def premiumSettlementDay(tradeDay: Day) = tradeDay.addBusinessDays(businessCalendar, 5)
}

class UnknownTrinityMarketException(val code:String) extends Exception("Unknown trinity market: " + code)

case class LimSymbol(name: String, multiplier: Double = 1)

object CommodityMarket {
  val remap: Map[CommodityMarket, CommodityMarket] = Map(
    Market.ICE_WTI → Market.NYMEX_WTI,
    Market.ICE_RBOB → Market.NYMEX_GASOLINE,
    Market.ICE_HEATING → Market.NYMEX_HEATING
  )
}

object LimSymbol {
  def fromOpt(name: Option[String], multiplier: Double) = name.map(n => LimSymbol(n, multiplier))
}

class FuturesMarket(
        @transient name: String,
        @transient lotSize: Option[Double],
        @transient uom: UOM,
        @transient currency: UOM,
        @transient businessCalendar: BusinessCalendarSet,
        @transient eaiQuoteID: Option[Int],
        @transient pricesTable: ForwardCurvePricingTable,
        @transient tenor: TenorType,
        @transient val expiryRule: FuturesExpiryRule,
        @transient val exchange: FuturesExchange,
        @transient commodity : Commodity,
        @transient val hasOptions: Boolean,
        @transient val fcVolatilityCurveIDS : List[Int] = Nil,
        @transient limSymbol : Option[LimSymbol] = None,
        @transient precision : Option[Precision] = None
        ) extends CommodityMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, pricesTable, tenor, commodity, limSymbol, precision)
          with KnownExpiry {
  assert(commodity != null)

  override def priceRiskFactor(marketDayAndTime: DayAndTime, dateRange: DateRange): Option[RiskFactor] = {
    val offset = (tenor, dateRange) match {
      case (Month, m: Month) => m - frontMonth(marketDayAndTime)
      case (Day, d: Day) => d - frontPeriod(marketDayAndTime.day).asInstanceOf[Day]
      case _ => throw new Exception(name + ":tenor, " + tenor + ", and dateRange, " + dateRange + " don't match for " + this)
    }
    if(offset < 0) {
      None
    } else {
      Some(ForwardPriceRiskFactor(this, offset, offset))
    }
  }

  /**
   * Trinity stores Futures periods as days, regardless of the market
   * convention. This infers the corresponding period.
   */
  def convertTrinityMaturityDayToPeriod(maturityDay : Day) : DateRange = {
     commodity match {
      case Freight => {
        // Freight trades have the maturity date set to the settlement date
        // Settlement is usually the first tuesday but is sometimes wrong
        // TODO Jerome check settlement days
        // TODO Jerome these are probably FFAs or maybe cleared contracts, need to check
        // http://www.balticexchange.com/default.asp?action=article&ID=35
        maturityDay.containingMonth - 1
      }
      case _ => frontPeriod(lastTradingDayFromTrinityMaturity(maturityDay))
    }

  }

  def closeTime = exchange.closeTime

  /**
   * Another workaround for the fact that Trinity stores futures periods as days.
   */
  def lastTradingDayFromTrinityMaturity(maturityDay : Day) = tenor match {
    case Day => lastTradingDay(maturityDay)
    case Month => maturityDay
  }

}

/**
 * A forward market is very similar to a futures market but without expiry rules, exchanges etc.
 * Currently the only examples we have (other than ProxyForwardMarkets) are LBMA gold and silver
 * plus various oil indices
 */
class ForwardMarket(
  @transient name: String,
  @transient lotSize: Option[Double],
  @transient uom: UOM,
  @transient currency: UOM,
  @transient businessCalendar: BusinessCalendarSet,
  @transient eaiQuoteID: Option[Int],
  @transient pricesTable: ForwardCurvePricingTable,
  @transient tenor: TenorType,
  @transient commodity: Commodity,
  @transient limSymbol: Option[LimSymbol] = None,
  @transient precision : Option[Precision] = None
)
  extends CommodityMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, pricesTable, tenor, commodity, limSymbol, precision)
{
  def priceRiskFactor(marketDayAndTime: DayAndTime, dateRange: DateRange) = {
    val offset = (tenor, dateRange) match {
      case (Day, d: Day) => d - marketDayAndTime.day
      case _ => throw new Exception(name + ":tenor, " + tenor + ", and dateRange, " + dateRange + " don't match for " + this)
    }
    if(offset < 0) {
      None
    } else {
      Some(ForwardPriceRiskFactor(this, offset, offset))
    }
  }

  def settlementDay(deliveryDay:Day) = deliveryDay

  /**
   * This is needed because sometimes we need to translate the day into a front period.
   */
  def underlying(day: Day): DateRange = day

  /**
   * This is needed because sometimes we need to translate the day into a front option period.
   */
  def underlyingOption(day: Day): DateRange = day

  override def nthPeriod(dayAndTime: DayAndTime, numPeriodsAhead: Int): Day = tenor match {
    case Day => dayAndTime.day + numPeriodsAhead
  }
}

/**
 *   A proxy forward market is just a proxy for a futures market.
 *
 * Commodity Forward trades are booked against Futures markets. This allows a rough mtm
 * by using an appropriate price from the futures market. Most of our futures markets
 * are monthly so having forwards (with a single day for delivery) means the forward
 * market has to decide which price to use.
 */
case class ProxyForwardMarket(proxy: FuturesMarket) extends ForwardMarket(proxy.name, proxy.lotSize, proxy.uom,
  proxy.currency, proxy.businessCalendar, proxy.eaiQuoteID, proxy.pricesTable, proxy.tenor, proxy.commodity) {

  override def priceRiskFactor(marketDayAndTime: DayAndTime, dateRange: DateRange): Option[RiskFactor] = proxy.priceRiskFactor(marketDayAndTime, dateRange)

  override def underlying(day: Day) = proxy.frontPeriod(day)

  override def underlyingOption(day: Day) = proxy.frontOptionPeriod(day)
}


trait IsBrentMonth{
  def month : BrentMonth
  def marketDataKey : MarketDataKey
}

/**
 * Used for turning Market identifiers into Market instances.
 *
 * Info like lot size, holidays etc can be found in dblQuotes in EAI
 */
object Market {
  import starling.curves.LinearInterpolation
  import starling.market.FuturesExchangeFactory._
  import starling.daterange._

  lazy val cals = new BusinessCalendars(HolidayTablesFactory.holidayTables)
  lazy val expiry = FuturesExpiryRuleFactory.expiryRules
  lazy val precisionRules = MarketPrecisionFactory.rules
  
  /**
   * Comex Markets
   */
  private def comexFuturesMarket(
    name: String,
    lotSize: Double,
    uom: UOM,
    eaiQuoteID: Int,
    expiryRule: FuturesExpiryRule,
    commodity: Commodity,
    limSymbol: String)
  = new FuturesMarket(name, Some(lotSize), uom, USD, cals.COMEX, Some(eaiQuoteID),
      MetalsPriceTable, Month, expiryRule, COMEX, commodity, true, limSymbol = Some(LimSymbol(limSymbol, 0.01)), precision = precisionRules.rule(eaiQuoteID)
    )

  lazy val COMEX_GOLD : FuturesMarket = comexFuturesMarket("COMEX Gold", 100, OZ, 546, expiry.COMEX_GOLD, Gold, "GC")
  lazy val COMEX_SILVER : FuturesMarket = comexFuturesMarket("COMEX Silver", 5000, OZ, 547, expiry.COMEX_SILVER, Silver, "SI")
  lazy val COMEX_PALLADIUM : FuturesMarket = comexFuturesMarket("COMEX Palladium", 100, OZ, 683, expiry.COMEX_PT_PA, Palladium, "COMEX.PAC")
  lazy val COMEX_PLATINUM : FuturesMarket = comexFuturesMarket("COMEX Platinum", 50, OZ, 684, expiry.COMEX_PT_PA, Platinum, "PL")
  lazy val COMEX_HIGH_GRADE_COPPER : FuturesMarket = comexFuturesMarket(
    "COMEX High Grade Copper", 25000, LB, 545, expiry.COMEX_HG_COPPER, Copper, "HG")

  lazy val EXBXG_MARKETS = List(  //http://www.exbxg.com/en/services.html
    ("CR", Steel, MT),    //304 Stainless Steel Cold Rolling Coil
    ("Ni", Nickel, KG),   //Electrolytic Nickel Plate
    ("HR", Steel, MT),    //304 Stainless Steel Hot Rolling Coil
    ("4FHR", Steel, MT),   //4 Feet Stainless Steel Hot Rolling Coil
    ("Pb", Lead, MT),
    ("Sn", Tin, KG),
    ("In", Indium, KG),
    ("Co", Cobalt, MT),
    ("FeMo", Ferromolybdenum, KG)
   ).map { case(code, commodity, uom) => new FuturesMarket(
      "EXBXG " + code, Some(1.0), uom, CNY, cals.CHINESE_STATE_HOLIDAYS,
      None, null, Month, FuturesExpiryRule.Null, FuturesExchangeFactory.EXBXG, commodity, false
    ) }

  /**
   * LME Markets
   */
  private def lmeFuturesMarket(name: String,
                               lotSize: Double,
                               uom: UOM,
                               commodity: Commodity,
                               limSymbol: Option[String] = None) : FuturesMarket
  = new FuturesMarket(
    name, Some(lotSize), uom, USD, cals.LME, None, MetalsPriceTable, Day,
    expiry.LME, LME, commodity, true, limSymbol = LimSymbol.fromOpt(limSymbol, 1), precision = None
  )
    with HasInterpolation {
    def interpolation = LinearInterpolation

    /**
     * 2 days to cash
     * http://www.lme.com/6444.asp
     */
    override def frontPeriod(day: Day) = day.addBusinessDays(cals.LME, 2)

    /**
     * Options on LME exercise into a future on the 3rd Wednesday of the month
     */
    override def frontOptionPeriod(day: Day) = {
      var period = day
      while (optionExpiry(period) != period) {
        period = period match {
          case d: Day => d + 1
        }
      }
      period.containingMonth.thirdWednesday
    }

    override def nthOptionPeriod(dayAndTime: DayAndTime, numPeriodsAhead: Int) = {
      val period = frontOptionPeriod(dayAndTime.day)
      frontOptionPeriod((period.firstMonth + numPeriodsAhead).firstDay)
    }
  }

  lazy val LME_ALUMINIUM_ALLOY = lmeFuturesMarket("LME Aluminium Alloy", 20, MT, AluminiumAlloy)
  lazy val LME_ALUMINIUM = lmeFuturesMarket("LME Aluminium", 25, MT, Aluminium,limSymbol = Some("LCH.LME.ALUMINIUM"))
  lazy val LME_STEEL_BILLETS = lmeFuturesMarket("LME Steel Mediterranean Billets", 65, MT, Steel)
  lazy val LME_COBALT = lmeFuturesMarket("LME Cobalt", 1, MT, Cobalt)
  lazy val LME_COPPER = lmeFuturesMarket("LME Copper", 25, MT, Copper, limSymbol = Some("LCH.LME.COPPER"))
  lazy val LME_LEAD = lmeFuturesMarket("LME Lead", 25, MT, Lead, limSymbol = Some("LCH.LME.LEAD"))
  lazy val LME_NICKEL = lmeFuturesMarket("LME Nickel", 6, MT, Nickel, limSymbol = Some("LCH.LME.NICKEL"))
  lazy val LME_NASAAC = lmeFuturesMarket("LME NASAAC", 20, MT, NASAAC)
  lazy val LME_ZINC = lmeFuturesMarket("LME Zinc", 25, MT, Zinc, limSymbol = Some("LCH.LME.ZINC"))
  lazy val LME_TIN = lmeFuturesMarket("LME Tin", 5, MT, Tin, limSymbol = Some("LCH.LME.TIN"))
  lazy val LME_MOLYBDENUM = lmeFuturesMarket("LME Molybdenum", 6, MT, Molybdenum)

  case class FreightLimSymbol(rootName : String, tenor : TenorType, nPeriod : Int){
    private def periodText = {
      (tenor, nPeriod) match {
        case (`Month`, 0) => "CURMON"
        case (`Month`, i) => i + "MON"
        case (`Quarter`, 0) => "CURQ"
        case (`Quarter`, i) => i + "Q"
        case (`Year`, i) => i + "CAL"
      }
    }
    def name = rootName + "." + periodText
  }
  /**
   * Baltic Exchange markets
   */
  class BalticFuturesMarket(
    name: String,
    uom: UOM,
    eaiQuoteID: Int,
    limRootName : String)
    extends FuturesMarket(
      name, None, uom, USD, cals.BALTIC, Some(eaiQuoteID),
      MetalsPriceTable, Month, expiry.BALTIC, BALTIC, Freight, false, limSymbol = Some(LimSymbol(limRootName))
      ) {
    override def observationDays(period: DateRange) = {
      observationDayCache.memoize(period, {
        p: DateRange => DateRange(p.firstDay, lastTradingDay(p.firstMonth)).toList.filter(isObservationDay).intersect(period.toList)
      })
    }
    def limSymbols = (0 to 9).toList.map(FreightLimSymbol(limRootName, Month, _)) ++
      (0 to 4).toList.map(FreightLimSymbol(limRootName, Quarter, _)) ++
      (1 to 5).toList.map(FreightLimSymbol(limRootName, Year, _))

    def periodForLimSymbol(dayAndTime : DayAndTime, ls : FreightLimSymbol) : DateRange = {
      val frontMonth = {
        val month: Month = dayAndTime.containingMonth
        if (dayAndTime >= lastTradingDay(month).endOfDay)
          month + 1
        else
          month
      }
      ls.tenor match {
        case `Month` => frontMonth + ls.nPeriod
        case `Quarter` => frontMonth.firstDay.containingQuarter + ls.nPeriod
        case `Year` => frontMonth.firstDay.containingYear + ls.nPeriod
      }
    }
  }

  lazy val BALTIC_PANAMAX = new BalticFuturesMarket("Baltic Panamax TC Avg", DAY, 511, "BALTIC.PANAMAX.TC4")
  lazy val BALTIC_SUPRAMAX = new BalticFuturesMarket("Baltic Supramax TC Avg", DAY, 1306, "BALTIC.SUPRAMAX.TC5")
  lazy val BALTIC_CAPESIZE = new BalticFuturesMarket("Baltic Capesize TC Avg", DAY, 512, "BALTIC.CAPESIZE.TC4")
  lazy val BALTIC_HANDYMAX = new BalticFuturesMarket("Baltic Handymax TC Avg", DAY, 1305, "BALTIC.HANDYSIZE.TC")
  lazy val BALTIC_CAPESIZE_C7 = new BalticFuturesMarket("Baltic Capesize C7 Bolivar to Rotterdam", MT, 524, "BALTIC.CAPESIZE.ROUTE.C7"){
    override def observationDays(period: DateRange) = {
      observationDayCache.memoize(period, {
        p:DateRange => {
          DateRange(p.firstDay, lastTradingDay(p.firstMonth)).toList.filter(isObservationDay).takeRight(7)
        }
      })
    }
  }




  /**
   * Shanghai futures exchange markets
   */
  private def shanghaiFuturesMarket(
    name: String,
    lotSize: Double,
    uom: UOM,
    eaiQuoteID: Int,
    commodity: Commodity,
    limSymbol: Option[String] = None)
  = new FuturesMarket(
      name, Some(lotSize), uom, CNY, cals.SFS, Some(eaiQuoteID), MetalsPriceTable,
      Month, expiry.SHANGHAI, SFS, commodity, true, limSymbol = LimSymbol.fromOpt(limSymbol, 1), precision = precisionRules.rule(eaiQuoteID)
    )

  lazy val SHANGHAI_ZINC = shanghaiFuturesMarket("Shanghai Zinc", 5, MT, 1037, Zinc, limSymbol = Some("ZN"))
  lazy val SHANGHAI_COPPER = shanghaiFuturesMarket("Shanghai Copper", 5, MT, 973, Copper, limSymbol = Some("CU"))
  lazy val SHANGHAI_ALUMINUIUM = shanghaiFuturesMarket("Shanghai Aluminium", 5, MT, 974, Aluminium, limSymbol = Some("AL"))
  lazy val SHANGHAI_GOLD = shanghaiFuturesMarket("Shanghai Gold", 1000, G, 1148, Gold, limSymbol = Some("AU"))
  lazy val STEEL_REBAR_SHANGHAI = shanghaiFuturesMarket("Steel Rebar Shanghai", 10, MT, 1441, Steel)
  lazy val IRON_ORE = shanghaiFuturesMarket("Iron Ore", 10, MT, 1441, IronOre)

  /**
   * Nymex Markets
   */
  private def nymexFuturesMarket(
    name: String,
    lotSize: Double,
    uom: UOM,
    eaiQuoteID: Int,
    commodity: Commodity,
    expiryRule: FuturesExpiryRule,
    limSymbol: Option[LimSymbol] = None,
    volID: Option[Int] = None)
  = new FuturesMarket(name, Some(lotSize), uom, USD, cals.NYMEX, Some(eaiQuoteID),
    NonMetalsPriceTable, Month, expiryRule, NYMEX, commodity, true,
    limSymbol = limSymbol, precision = precisionRules.rule(eaiQuoteID)) {
    override def volatilityID = volID
  }

  lazy val NYMEX_WTI = nymexFuturesMarket("NYMEX WTI", 1000, BBL, 2, WTI, expiry.rule(2), limSymbol = Some(LimSymbol("CL")), volID = Some(25))
  lazy val NYMEX_BRENT = nymexFuturesMarket("NYMEX Brent", 1000, BBL, 605, Brent, expiry.rule(Market.ICE_BRENT.eaiQuoteID.get), limSymbol = Some(LimSymbol("PCAJG00", 1)), volID = Some(20))
  lazy val NYMEX_HEATING = nymexFuturesMarket("NYMEX Heat", 42000, GAL, 15, HeatingOil, expiry.rule(15), volID = Some(22), limSymbol = Some(LimSymbol("HO")))
  lazy val NYMEX_SINGAPORE_FUEL_OIL = nymexFuturesMarket("NYMEX Singapore 380 Fuel Oil", 100, MT, 1310, FuelOil(6.35), expiry.rule(1310), limSymbol = Some(LimSymbol("HZ", 1)))
  lazy val NYMEX_GASOLINE = nymexFuturesMarket("RBOB", 42000, GAL, 880, Gasoline(8.33), expiry.rule(880), volID = Some(23), limSymbol = Some(LimSymbol("RB.UNL", 0.01)))
  lazy val NYMEX_NAT_GAS = nymexFuturesMarket("NYMEX Nat Gas", 1000, MMBTU, 140, NatGas, expiry.rule(140))


  /*
   * ICE Markets
   */
  private def iceFuturesMarket(
    name: String,
    lotSize: Double,
    uom: UOM,
    eaiQuoteID: Int,
    commodity: Commodity,
    expiry: FuturesExpiryRule,
    limSymbol: Option[String] = None,
    volID: Option[Int] = None)
  = new FuturesMarket(name, Some(lotSize), uom, USD, cals.ICE, Some(eaiQuoteID),
    NonMetalsPriceTable, Month, expiry, ICE, commodity, true,
    limSymbol = LimSymbol.fromOpt(limSymbol, 1), precision = precisionRules.rule(eaiQuoteID)) {
    override def volatilityID = volID
  }

  lazy val ICE_BRENT = iceFuturesMarket("ICE Brent", 1000, BBL, 1, Brent, expiry.rule(1), limSymbol = Some("FB"), Some(20))
  lazy val ICE_GAS_OIL = iceFuturesMarket("ICE Gas Oil", 100, MT, 14, GasOil(7.45), expiry.rule(14), limSymbol = Some("FP"), Some(21))
  lazy val ICE_WTI = iceFuturesMarket("ICE WTI", 1000, BBL, 890, WTI, expiry.ICE_WTI(cals.ICE, expiry.rule(890)), limSymbol = Some("CL"), volID = Some(25))
  lazy val ICE_RBOB = iceFuturesMarket("ICE RBOB", 42000, GAL, 913, Gasoline(8.33), expiry.rule(913), volID = Some(23))
  lazy val ICE_HEATING = iceFuturesMarket("ICE Heat", 42000, GAL, 914, HeatingOil, expiry.rule(914), volID = Some(22))

  /**
   * MDEX Crude Palm Oil
   */
  lazy val MDEX_CRUDE_PALM_OIL = new FuturesMarket("MDEX Crude Palm Oil", Some(25.0), MT, MYR, cals.KLS, Some(1125),
    NonMetalsPriceTable, Month, expiry.rule(1125), ICE, PalmOil, true, precision = precisionRules.rule(1125))

  /**
   * Dubai Crude - I can't find much about this. I'm not sure where the futures contracts are traded.
   * I'm guessing at the expiry rule based on what the FCA has
   * TODO Jerome
   */
  lazy val PLATTS_DUBAI = new FuturesMarket("Platts Dubai", Some(1000.0), BBL, USD, cals.PLD, Some(141),
    NonMetalsPriceTable, Month, expiry.dubaiCrudeExpiryRule, NYMEX, DubaiCrude, false, limSymbol = Some(LimSymbol("PCAAT00")), precision = precisionRules.rule(141))


  /**
   * Another platts 'futures' market that I don't understand
   * TODO Jerome
   */
  lazy val PLATTS_BRENT = new FuturesMarket("Platts Brent", None, BBL, USD, cals.PLE, Some(101),
    NonMetalsPriceTable, Month, expiry.plattsBrentExpiryRule, NYMEX, Brent, false, limSymbol = Some(LimSymbol("PCAAP00")), precision = precisionRules.rule(101))

  lazy val PLATTS_BRENT_MONTH_MARKETS = (1 to 12).map{
    i => val brentMonth = BrentMonth(i)
    val market = new ForwardMarket(
      "Platts " + brentMonth, 
      None, 
      PLATTS_BRENT.uom, 
      PLATTS_BRENT.currency, 
      PLATTS_BRENT.businessCalendar, 
      PLATTS_BRENT.eaiQuoteID, 
      NonMetalsPriceTable, 
      Day, 
      Brent,
      limSymbol = Some(LimSymbol("PLATTS_BRENT." + brentMonth.monthName)), precision = precisionRules.rule(PLATTS_BRENT.eaiQuoteID.get)) with IsBrentMonth with HasInterpolation{
        def month = brentMonth
        def marketDataKey = PLATTS_BRENT.curveKey.marketDataKey
        def interpolation = InverseConstantInterpolation
      } 

    brentMonth → market
  }.toMap

  /**
   * Another wrong market. What's the expiry rules??
   */
  lazy val URALS_CIF_MED = new FuturesMarket("Urals CIF Med", None, BBL, USD, cals.PLATTS_EUROPEAN_CRUDE, Some(96),
    NonMetalsPriceTable, Month, expiry.plattsBrentExpiryRule, NYMEX, Brent, false, limSymbol = Some(LimSymbol("PCAAS00")), precision = precisionRules.rule(96))

  /**
   * Forward Markets
   */


  /**
   * Distilates
   */
  lazy val FUEL_FOB_ROTTERDAM_BARGES_3_5 = new ForwardMarket("3.5% Fuel FOB Rotterdam Barges", Some(1000.0), MT, USD, cals.PLE, Some(5),
                                                NonMetalsPriceTable, Day, FuelOil(6.35), Some(LimSymbol("PUABC00")), precision = precisionRules.rule(5)) with HasInterpolation {
    override def volatilityID = Some(18)

    def interpolation = InverseConstantInterpolation // TODO Jerome FCA has monthly forward prices but we need daily - this isn't right
  }
  lazy val PREM_UNL_FOB_ROTTERDAM_BARGES = new ForwardMarket("Prem Unl FOB Rotterdam Barges", Some(1000.0), MT, USD, cals.ICE, Some(17),
    NonMetalsPriceTable, Day, FuelOil(6.35), limSymbol = Some(LimSymbol("PGABM00")), precision = precisionRules.rule(17)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation // TODO Jerome FCA has monthly forward prices but we need daily - this isn't right
  }
  lazy val FUEL_FOB_NWE_CARGOES_1 = new ForwardMarket("1% Fuel FOB NWE Cargoes", Some(1000.0), MT, USD, cals.PLE, Some(3),
    NonMetalsPriceTable, Day, FuelOil(6.35), limSymbol = Some(LimSymbol("PUAAM00")), precision = precisionRules.rule(3)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val NAPHTHA_CIF_NWE_CARGOES = new ForwardMarket("Naphtha CIF NWE Cargoes", Some(1000.0), MT, USD, cals.PLE, Some(37),
    NonMetalsPriceTable, Day, Naphtha(8.9), limSymbol = Some(LimSymbol("PAAAL00")), precision = precisionRules.rule(37)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val GAS_OIL_0_5_SINGAPORE = new ForwardMarket("Gas Oil 0.5 Singapore", Some(1000.0), BBL, USD, cals.PLD, Some(52),
    NonMetalsPriceTable, Day, GasOil(7.45), limSymbol = Some(LimSymbol("POABC00")), precision = precisionRules.rule(52)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val MOGAS_95_UNL_10PPM_NWE_BARGES = new ForwardMarket("Mogas 95 Unl 10ppm NWE Barges (Argus)", Some(1000.0), MT, USD, cals.ARE, Some(88),
    NonMetalsPriceTable, Day, Gasoline(8.33), limSymbol = Some(LimSymbol("GPTZNESB")), precision = precisionRules.rule(88)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val UNL_92_SINGAPORE_CARGOES = new ForwardMarket("Unl 92 Singapore Cargoes", Some(1000.0), BBL, USD, cals.PLD, Some(198),
    NonMetalsPriceTable, Day, Gasoline(8.33), limSymbol = Some(LimSymbol("PGAEY00")), precision = precisionRules.rule(198)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val GAS_OIL_0_1_FOB_ROTTERDAM_BARGES = new ForwardMarket("Gas Oil 0.1% FOB Rotterdam Barges (Platts)", None, MT, USD, cals.PLE, Some(1011),
    NonMetalsPriceTable, Day, GasOil(7.45), limSymbol = Some(LimSymbol("AAYWT00")), precision = precisionRules.rule(1011)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val GAS_OIL_0_1_CIF_NWE_CARGOES = new ForwardMarket("Gas Oil 0.1% CIF NWE Cargoes (Platts)", None, MT, USD, cals.PLE, Some(1049),
    NonMetalsPriceTable, Day, GasOil(7.45), limSymbol = Some(LimSymbol("AAYWS00")), precision = precisionRules.rule(1049)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val PREM_UNL_10PPM_FOB_MED_CARGOES = new ForwardMarket("Prem Unl 10ppm FOB Med Cargoes (Platts)", Some(1000.0), MT, USD, cals.PLE, Some(1183),
    NonMetalsPriceTable, Day, Gasoline(8.33), limSymbol = Some(LimSymbol("AAWZA00")), precision = precisionRules.rule(1183)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val PREM_UNL_EURO_BOB_OXY_NWE_BARGES = new ForwardMarket("Prem Unl Euro-Bob Oxy NWE Barges (Argus)", Some(1000.0), MT, USD, cals.ARE, Some(1312),
    NonMetalsPriceTable, Day, Gasoline(8.33), limSymbol = Some(LimSymbol("GP5643A0")), precision = precisionRules.rule(1312)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val JET_CIF_NWE_CARGOES = new ForwardMarket("Jet CIF NWE Cargoes", Some(1000.0), MT, USD, cals.PLE, Some(18),
    NonMetalsPriceTable, Day, JetKero(7.878), limSymbol = Some(LimSymbol("PJAAU00")), precision = precisionRules.rule(18)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }
  lazy val GAS_OIL_ULSD_10PPM_CIF_NWE_CARGOES = new ForwardMarket("Gas Oil ULSD 10ppm CIF NWE Cargoes", None, MT, USD, cals.PLE, Some(598),
    NonMetalsPriceTable, Day, GasOil(7.45), limSymbol = Some(LimSymbol("AAVBG00")), precision = precisionRules.rule(598)) with HasInterpolation {
    def interpolation = LinearInterpolation
  }
  lazy val GAS_OIL_ULSD_10PPM_FOB_ROTTERDAM_BARGES = new ForwardMarket("Gas Oil ULSD 10ppm FOB Rotterdam Barges", Some(1000.0), MT, USD, cals.PLE, Some(883),
    NonMetalsPriceTable, Day, GasOil(7.45), limSymbol = Some(LimSymbol("AAJUS00")), precision = precisionRules.rule(883)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }

  lazy val DATED_BRENT = new ForwardMarket("Dated Brent", Some(1000.0), BBL, USD, cals.PLE, Some(40),
    NonMetalsPriceTable, Day, Brent, limSymbol = Some(LimSymbol("PCAAS00")), precision = precisionRules.rule(40)) with HasInterpolation {
    def interpolation = InverseConstantInterpolation
  }

  /**
   * A swap based on the Platts daily assessment price for FOB Gulf Coast for No.6 3% (Waterborne).
   */
  lazy val No_6_3PC_USGC_Waterborne = new ForwardMarket("No.6 3% USGC Waterborne", Some(1000.0), BBL, USD, cals.PLH, Some(11),
    NonMetalsPriceTable, Day, FuelOil(6.35), limSymbol = Some(LimSymbol("PUAFZ00")), precision = precisionRules.rule(11)) with HasInterpolation {
    def interpolation = LinearInterpolation // TODO Jerome FCA has monthly forward prices but we need daily - this isn't right

    override def volatilityID = Some(26)
  }

  /**
   * A swap based on the Platts daily assessment price for Unl 87 USGC Pipeline
   */
  lazy val UNL_87_USGC_PIPELINE = new ForwardMarket("Unl 87 USGC Pipeline", Some(42000), GAL, USD, cals.PLH, Some(34),
    NonMetalsPriceTable, Day, new Gasoline(8.33), limSymbol = Some(LimSymbol("PGACT00", 0.01)), precision = precisionRules.rule(34)) with HasInterpolation {
    def interpolation = LinearInterpolation // TODO Jerome FCA has monthly forward prices but we need daily - this isn't right
  }

  lazy val HSFO_180_CST_Singapore = new ForwardMarket("HSFO 180 CST Singapore", Some(1000.0), MT, USD, cals.PLD, Some(8),
    NonMetalsPriceTable, Day, FuelOil(6.5), limSymbol = Some(LimSymbol("PUADV00")), precision = precisionRules.rule(8)) with HasInterpolation {
    def interpolation = LinearInterpolation // TODO Jerome FCA has monthly forward prices but we need daily - this isn't right

    override def volatilityID = Some(36)
  }
  lazy val HSFO_380_CST_Singapore = new ForwardMarket("HSFO 380 CST Singapore", Some(1000.0), MT, USD, cals.PLD, Some(134),
    NonMetalsPriceTable, Day, FuelOil(6.35), limSymbol = Some(LimSymbol("PPXDK00")), precision = precisionRules.rule(134)) with HasInterpolation {
    def interpolation = LinearInterpolation // TODO Jerome FCA has monthly forward prices but we need daily - this isn't right
  }

  // CME EuroDollar market only needs a calendar
  val CME_EURO_DOLLAR_FUTURES_MARKET = cals.UK

  private val UNKNOWN_TRINITY_CODE = "???"
  // Map of trinity codes to Markets
  lazy val trinityCodeMap: Map[String, CommodityMarket] = Map(
    "GC" → COMEX_GOLD, "SI" → COMEX_SILVER, "XGC" → COMEX_GOLD, "XSI" → COMEX_SILVER, "XPD" → COMEX_PALLADIUM,
    "XPT" → COMEX_PLATINUM, "XHG" → COMEX_HIGH_GRADE_COPPER, "PL" → COMEX_PLATINUM, "PA" → COMEX_PALLADIUM,

    "XAA" → LME_ALUMINIUM_ALLOY, "XAL" → LME_ALUMINIUM, "XCO" → LME_COBALT, "XCU" → LME_COPPER, "XPB" → LME_LEAD,
    "XNI" → LME_NICKEL, "XNA" → LME_NASAAC, "XSN" → LME_TIN, "XZN" → LME_ZINC, "XFM" → LME_STEEL_BILLETS, UNKNOWN_TRINITY_CODE → LME_MOLYBDENUM,

    "XZS" → SHANGHAI_ZINC, "XCS" → SHANGHAI_COPPER, "XAS" → SHANGHAI_ALUMINUIUM, "SAU" → SHANGHAI_GOLD, "FRB" → STEEL_REBAR_SHANGHAI,

    "BPM" → BALTIC_PANAMAX, "BSM" → BALTIC_SUPRAMAX, "BCM" → BALTIC_CAPESIZE, "BC7" → BALTIC_CAPESIZE_C7, /*"BHM" → BALTIC_HANDYMAX, Fix if we care about var */

    "NCL" → NYMEX_WTI, "NHO" → NYMEX_HEATING, "NFO" → NYMEX_SINGAPORE_FUEL_OIL, "NNG" → NYMEX_NAT_GAS, "NHU" → NYMEX_GASOLINE,

    "ICO" → ICE_BRENT, "IGO" → ICE_GAS_OIL, "ICL" → ICE_WTI,

    "XFE" → IRON_ORE, "NPC" → NAPHTHA_CIF_NWE_CARGOES)

  lazy val marketToTrinityCode = trinityCodeMap.map(_.swap)

  lazy val eaiCodeMap: Map[Int, CommodityMarket] = eaiAliases ++ markets.toMapWithSomeKeys(_.eaiQuoteID)

  /**
   * Some trades are booked against market that are really just aliases to other. E.g. "IPE Brent 1st month"
   */
  lazy val eaiAliases = Map(1281 → ICE_BRENT, // ICE Brent 1st month Asia 1 minute marker
                            28 → ICE_BRENT, // ICE Brent 1st month
                            7 → NYMEX_WTI, // NYMEX WTI 1st month
                            1431 → ICE_GAS_OIL, // NYMEX Gas Oil 1st month
                            933 → NYMEX_GASOLINE, // NYMEX RBOB 1st month
                            29 → NYMEX_HEATING // NYMEX Heat 1st month
    )

  // markets not in a trinity map or an eai aliases map
  private lazy val otherMarkets = List(FUEL_FOB_ROTTERDAM_BARGES_3_5, HSFO_180_CST_Singapore, MDEX_CRUDE_PALM_OIL, PLATTS_DUBAI,
    DATED_BRENT, FUEL_FOB_ROTTERDAM_BARGES_3_5, PREM_UNL_FOB_ROTTERDAM_BARGES, FUEL_FOB_NWE_CARGOES_1, NAPHTHA_CIF_NWE_CARGOES,
    GAS_OIL_0_5_SINGAPORE, MOGAS_95_UNL_10PPM_NWE_BARGES, UNL_92_SINGAPORE_CARGOES, GAS_OIL_0_1_FOB_ROTTERDAM_BARGES,
  GAS_OIL_0_1_CIF_NWE_CARGOES, PREM_UNL_10PPM_FOB_MED_CARGOES, PREM_UNL_EURO_BOB_OXY_NWE_BARGES, HSFO_380_CST_Singapore,
    JET_CIF_NWE_CARGOES, GAS_OIL_ULSD_10PPM_CIF_NWE_CARGOES, GAS_OIL_ULSD_10PPM_FOB_ROTTERDAM_BARGES, PLATTS_BRENT,
    No_6_3PC_USGC_Waterborne, UNL_87_USGC_PIPELINE, NYMEX_BRENT, URALS_CIF_MED, ICE_RBOB, ICE_HEATING) ::: EXBXG_MARKETS
  
  lazy val markets:List[CommodityMarket] =
    (Set() ++ trinityCodeMap.valuesIterator ++ otherMarkets ++ PLATTS_BRENT_MONTH_MARKETS.values).toList.sortWith(_.name < _.name)

  val unusedMarketsWithOldData:List[CommodityMarket] = List(BALTIC_HANDYMAX)

  lazy val forwardMarkets:List[ForwardMarket] = (Set() ++ markets.flatMap{case (m: ForwardMarket) => Some(m); case _ => None}).toList.sortWith(_.name < _.name)
  lazy val futuresMarkets:List[FuturesMarket] = (Set() ++ markets.flatMap{case (m: FuturesMarket) => Some(m); case _ => None}).toList.sortWith(_.name < _.name)

  def marketsForExchange(exchange:FuturesExchange) = futuresMarkets.filter(_.exchange==exchange)

  def fromTrinityCode(code : String): CommodityMarket = {
    trinityCodeMap.get(code.trim) match {
      case Some(market) => market
      case None => throw new UnknownTrinityMarketException(code)
    }
  }

  def futuresMarketFromTrinityCode(code : String) : FuturesMarket = fromTrinityCode(code) match {
    case m: FuturesMarket => m
    case o => throw new Exception("Market " + o + " is not a FuturesMarket")
  }

  def fromEAIQuoteID(id: Int) = eaiCodeMap(id)

  def futuresMarketFromEAIQuoteID(id: Int) = commodityMarketOptionFromEAIQuoteID(id) match {
    case Some(f: FuturesMarket) => f
    case None => throw new Exception("No futures market for " + id)
  }

  def commodityMarketOptionFromEAIQuoteID(id: Int) = eaiCodeMap.get(id)

  def unapply(eaiQuoteID: Int):Option[Market] = commodityMarketOptionFromEAIQuoteID(eaiQuoteID) 

  /**
   * Find the market for a given market name
   * TODO: better place to implement? should maybe try and get this resolved - do we even need identifiers any more
   * given that the market names can be checked for uniqueness?
   */
  private val usedOrUnusedMarkets = markets ++ unusedMarketsWithOldData
  def fromName(marketName : String) : CommodityMarket = usedOrUnusedMarkets.find(_.name.equalsIgnoreCase(marketName)) match {
    case None => {
      val nameWithoutIdentifier = new Regex(" - [0-9]*$").replaceFirstIn(marketName, "")
      markets.find(_.name.equalsIgnoreCase(nameWithoutIdentifier)) match {
        case None => throw new IllegalStateException("No known market with name '" + marketName + "' or '" + nameWithoutIdentifier + "'")
        case Some(market) => market
      }
    }
    case Some(market) => market
  }

  def futuresMarketFromName(marketName : String): FuturesMarket = fromName(marketName) match {
    case m: FuturesMarket => m
    case o => throw new Exception("Market " + o + " is not a FuturesMarket")
  }

  def forwardMarketFromName(marketName : String): ForwardMarket = forwardMarkets.find(_.name.equalsIgnoreCase(marketName)) match {
    case None => {
      val market = futuresMarketFromName(marketName)
      new ProxyForwardMarket(market) // TODO hack until we figure out what trinity means with forward on futures markets
    }
    case Some(market) => market
  }


}

object FuturesMarket {
  def fromName(name:String) = Market.futuresMarketFromName(name)
  def testMarket(name : String, currency : UOM, uom : UOM) : FuturesMarket = {
    testMarket(name, currency, uom, -1, Day)
  }
  def testMarket(name : String, currency : UOM, uom : UOM, tenor: TenorType) : FuturesMarket = {
    testMarket(name, currency, uom, -1, tenor)
  }
  def testMarket(name : String, currency : UOM, uom : UOM, id : Int, tenor: TenorType) : FuturesMarket = {
    new FuturesMarket(name, Some(1.0), uom, currency, NilCalendar, Some(id), NoPriceTable, tenor, new FuturesExpiryRule {
      def lastTradingDay(d: DateRange) = d.firstDay - 1
      override def expiryDay(d: DateRange) = d.firstDay - 1
    }, FuturesExchangeFactory.COMEX, Brent, false)
  }
  def testMarketWithInterpolation(name : String, currency : UOM, uom : UOM) : FuturesMarket = {
    new FuturesMarket(name, Some(1.0), uom, currency, NilCalendar, None, NoPriceTable, Day, new FuturesExpiryRule {
      def lastTradingDay(d: DateRange) = d.firstDay - 1
    }, FuturesExchangeFactory.COMEX, Brent, false) with HasInterpolation {
      def interpolation = LinearInterpolation
    }
  }
}

object ForwardMarket {
  def testMarket(name : String, currency : UOM, uom : UOM) : ForwardMarket = {
    testMarket(name, currency, uom, -1)
  }
  def testMarket(name : String, currency : UOM, uom : UOM, id : Int) : ForwardMarket = {
    new ProxyForwardMarket(FuturesMarket.testMarket(name, currency, uom, id, Day))
  }
}

object MarketMain {
  def main(args : Array[String]) {
//    println(Market.fromTrinityCode("BPM"))
//    println(Market.fromTrinityCode("BSM"))
//    println(Market.fromName("ICE Brent").uom)
  }
}


