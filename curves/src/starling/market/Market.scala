package starling.market

import rules.{PrecisionRules, Precision, MarketPrecisionFactory}
import starling.curves._
import scala.None
import starling.quantity.UOM._
import starling.db._
import starling.daterange._
import collection.immutable.Map
import starling.utils.cache.CacheFactory
import starling.varcalculator.{ForwardPriceRiskFactor, RiskFactor}
import starling.calendar._
import starling.utils.ImplicitConversions._
import util.matching.Regex
import starling.marketdata.{PriceFixingsHistoryDataKey, MarketDataKey}
import starling.quantity._

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
  @transient val businessCalendar : BusinessCalendar,
  @transient val eaiQuoteID : Option[Int],
  @transient val tenor: TenorType,
  @transient val commodity : Commodity,
  @transient val conversions: Conversions,
  @transient val limSymbol : Option[LimSymbol] = None,
  @transient val precision : Option[Precision] = None
)
  extends Market with HasImpliedVol with KnownObservation with FixingHistoryLookup
{
  val uomName = uom.toString

  override def equals(p1: Any) = p1 match {
    case et: CommodityMarket => eaiQuoteID == et.eaiQuoteID && name.equalsIgnoreCase(et.name)
    case _ => super.equals(p1)
  }
  
  def isObservationDay(day: Day) = businessCalendar.isBusinessDay(day)

  override def toString = name

  override def hashCode = name.hashCode ^ eaiQuoteID.hashCode

  def curveKey = ForwardCurveKey(this)

  def positionUOM = uom

  @transient val priceUOM = currency / uom

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
    value.in(uom)(conversions)
  }

  def convertUOM(volume : Quantity, uom : UOM) : Quantity = {
    convert(volume, uom) match {
      case Some(beqv) => beqv
      case None => throw new Exception(this + ": Couldn't convert from " + volume + " to " + uom)
    }
  }

  def premiumSettlementDay(tradeDay: Day) = tradeDay.addBusinessDays(businessCalendar, 5)

  def historicPrice(env: InstrumentLevelEnvironment, observationDay: Day, period: DateRange): Quantity = {
    require(observationDay.endOfDay <= env.marketDay, "Can't ask for historic rpice for " + this + " in the future")
    env.quantity(MarketFixingKey(this, observationDay, period)) match {
      case nq: NamedQuantity => {
        val fixed = new SimpleNamedQuantity(name + "." + period.toShortString + " Fixed", new Quantity(nq.value, nq.uom))
        SimpleNamedQuantity(observationDay.toString, fixed)
      }
      case q => q
    }
  }

  def forwardPrice(env: InstrumentLevelEnvironment, period: DateRange, ignoreShiftsIfPermitted: Boolean) = {
    env.quantity(ForwardPriceKey(this, period))
  }

  def fixing(slice: MarketDataSlice, observationDay: Day, storedFixingPeriod: Option[StoredFixingPeriod]) = storedFixingPeriod match {
    case Some(period) => {
      val key = PriceFixingsHistoryDataKey(this)
      slice.fixings(key, ObservationPoint(observationDay, ObservationTimeOfDay.Default))
        .fixingFor(Level.Close, period)
        .toQuantity
    }
    case _ => throw new IllegalArgumentException("storedFixingPeriod not defined")
  }
}

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
        @transient businessCalendar: BusinessCalendar,
        @transient eaiQuoteID: Option[Int],
        @transient tenor: TenorType,
        @transient val expiryRule: FuturesExpiryRule,
        @transient val exchange: FuturesExchange,
        @transient commodity : Commodity,
        @transient conversions: Conversions = Conversions.default,
        @transient override val volatilityID : Option[Int] = None,
        @transient limSymbol : Option[LimSymbol] = None,
        @transient precision : Option[Precision] = None
        ) extends CommodityMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, tenor, commodity, conversions, limSymbol, precision)
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
        // TODO [07 Jul 2010] Jerome check settlement days
        // TODO [07 Jul 2010] Jerome these are probably FFAs or maybe cleared contracts, need to check
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

  val hasOptions = volatilityID.isDefined
}

class LMEFuturesMarket(
        @transient name: String,
        @transient lotSize: Option[Double],
        @transient uom: UOM,
        @transient currency: UOM,
        @transient businessCalendar: BusinessCalendar,
        @transient eaiQuoteID: Option[Int],
        @transient tenor: TenorType,
        @transient expiryRule: FuturesExpiryRule,
        @transient exchange: FuturesExchange,
        @transient commodity : Commodity,
        @transient conversions: Conversions = Conversions.default,
        @transient volatilityID : Option[Int] = None,
        @transient limSymbol: Option[LimSymbol] = None,
        @transient precision: Option[Precision] = None
                        ) extends FuturesMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, tenor, expiryRule, exchange, commodity, conversions, volatilityID, limSymbol, precision)
with HasInterpolation {
  def interpolation = LinearInterpolation

  /**
   * 2 days to cash
   * http://www.lme.com/6444.asp
   */
  override def frontPeriod(day: Day) = day.addBusinessDays(businessCalendar, 2)

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

/**
 * A Swap Market is a market used for forward prices for Swaps
 */
class SwapMarket(
  @transient name: String,
  @transient lotSize: Option[Double],
  @transient uom: UOM,
  @transient currency: UOM,
  @transient businessCalendar: BusinessCalendar,
  @transient eaiQuoteID: Option[Int],
  @transient commodity: Commodity,
  @transient conversions: Conversions,
  @transient limSymbol: Option[LimSymbol] = None,
  @transient precision : Option[Precision] = None
)
  extends CommodityMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, Day, commodity, conversions, limSymbol, precision)
  with HasInterpolation {
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

  override def nthPeriod(dayAndTime: DayAndTime, numPeriodsAhead: Int): Day = tenor match {
    case Day => dayAndTime.day + numPeriodsAhead
  }
  
  def interpolation = InverseConstantInterpolation
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
  private def provider = MarketProvider.provider
  lazy val cals: BusinessCalendars = new BusinessCalendars(HolidayTablesFactory.holidayTables)

  lazy val all: List[CommodityMarket] = provider.allFuturesMarkets ::: Index.publishedIndexes
  lazy val futuresMarkets = provider.allFuturesMarkets

  lazy val COMEX_GOLD : FuturesMarket = futuresMarketFromName("COMEX Gold")
  lazy val COMEX_SILVER : FuturesMarket = futuresMarketFromName("COMEX Silver")
  lazy val COMEX_PLATINUM : FuturesMarket = futuresMarketFromName("COMEX Platinum")
  lazy val COMEX_PALLADIUM : FuturesMarket = futuresMarketFromName("COMEX Palladium")
  lazy val COMEX_HIGH_GRADE_COPPER : FuturesMarket = futuresMarketFromName("COMEX High Grade Copper")



  //http://www.exbxg.com/en/services.html - called WUXI at Traf - no idea why as google knows nothing of this
  lazy val EXBG_STEEL_CR = exbgFuturesMarket("CR", Steel, MT) //304 Stainless Steel Cold Rolling Coil
  lazy val EXBG_NICKEL = exbgFuturesMarket("Ni", Nickel, KG)  //Electrolytic Nickel Plate
  lazy val EXBG_STEEL_HR = exbgFuturesMarket("HR", Steel, MT) //304 Stainless Steel Hot Rolling Coil
  lazy val EXBG_STEEL_4FHR = exbgFuturesMarket("4FHR", Steel, MT) //4 Feet Stainless Steel Hot Rolling Coil
  lazy val EXBG_LEAD = exbgFuturesMarket("Pb", Lead, MT)
  lazy val EXBG_TIN = exbgFuturesMarket("Sn", Tin, KG)
  lazy val EXBG_INDIUM = exbgFuturesMarket("In", Indium, KG)
  lazy val EXBG_COBALT = exbgFuturesMarket("Co", Cobalt, MT)
  lazy val EXBG_FERROMOLYBDENIUM = exbgFuturesMarket("FeMo", Ferromolybdenum, KG)

  lazy val EXBXG_MARKETS = List(EXBG_STEEL_CR, EXBG_NICKEL, EXBG_STEEL_HR, EXBG_STEEL_4FHR, EXBG_LEAD, EXBG_TIN, EXBG_INDIUM, EXBG_COBALT, EXBG_FERROMOLYBDENIUM)
  def exbgFuturesMarket(code: String,commodity: Commodity, uom: UOM) = {
    new FuturesMarket(
      "EXBXG " + code, Some(1.0), uom, CNY, cals.CHINESE_STATE_HOLIDAYS,
      None, Month,
      new FuturesExpiryRule(){
        // TODO - find the correct rule - for now we're going with last working day of previous month
        val name = "EXBXG"

        def lastTradingDay(d: DateRange) = d.firstDay.previousBusinessDay(cals.CHINESE_STATE_HOLIDAYS)
      },
      FuturesExchangeFactory.EXBXG, commodity
    )
  }

  lazy val LME_ALUMINIUM_ALLOY = futuresMarketFromName("LME Aluminium Alloy")
  lazy val LME_ALUMINIUM = futuresMarketFromName("LME Aluminium")
  lazy val LME_COBALT = futuresMarketFromName("LME Cobalt")
  lazy val LME_COPPER = futuresMarketFromName("LME Copper")
  lazy val LME_LEAD = futuresMarketFromName("LME Lead")
  lazy val LME_NICKEL = futuresMarketFromName("LME Nickel")
  lazy val LME_ZINC = futuresMarketFromName("LME Zinc")
  lazy val LME_TIN = futuresMarketFromName("LME Tin")
  lazy val LME_STEEL_BILLETS = futuresMarketFromName("LME Steel Mediterranean Billets")
  lazy val LME_MOLYBDENUM = futuresMarketFromName("LME Molybdenum")
  lazy val LME_NASAAC = futuresMarketFromName("LME NASAAC")
  
  lazy val SHANGHAI_ZINC = futuresMarketFromName("Shanghai Zinc")
  lazy val SHANGHAI_COPPER = futuresMarketFromName("Shanghai Copper")
  lazy val SHANGHAI_ALUMINUIUM = futuresMarketFromName("Shanghai Aluminium")
  lazy val SHANGHAI_GOLD = futuresMarketFromName("Shanghai Gold")
  lazy val STEEL_REBAR_SHANGHAI = futuresMarketFromName("SHFE Steel Rebar")

  lazy val NYMEX_WTI = futuresMarketFromName("NYMEX WTI")
  lazy val NYMEX_BRENT = futuresMarketFromName("NYMEX Brent")
  lazy val NYMEX_HEATING = futuresMarketFromName("NYMEX Heat")
  lazy val NYMEX_SINGAPORE_FUEL_OIL = futuresMarketFromName("NYMEX Singapore 380 Fuel Oil")
  lazy val NYMEX_GASOLINE = futuresMarketFromName("Nymex RBOB")
  lazy val NYMEX_NAT_GAS = futuresMarketFromName("NYMEX Henry Hub Natural Gas")

  lazy val ICE_BRENT = futuresMarketFromName("IPE Brent")
  lazy val ICE_GAS_OIL = futuresMarketFromName("IPE Gas Oil")
  lazy val ICE_WTI = futuresMarketFromName("ICE WTI")
  lazy val ICE_RBOB = futuresMarketFromName("ICE RBOB")
  lazy val ICE_HEATING = futuresMarketFromName("ICE Heat")

  lazy val MDEX_CRUDE_PALM_OIL = futuresMarketFromName("MDEX Crude Palm Oil")

  lazy val PLATTS_DUBAI = futuresMarketFromName("Platts Dubai")

  def fromName(marketName: String): CommodityMarket = try {
    provider.futuresMarket(marketName) match {
      case Some(m) => m
      case None => Index.publishedIndexFromName(marketName)
    }
  } catch {
    case e => throw new Exception("No market: " + marketName)
  }
  
  def futuresMarketFromName(marketName: String): FuturesMarket = provider.futuresMarket(marketName).getOrElse(throw new Exception("No market: " + marketName))
  def futuresMarketFromQuoteID(id: Int): FuturesMarket = futuresMarketFromQuoteIDOption(id).getOrElse(throw new Exception("No market: " + id))
  def futuresMarketFromQuoteIDOption(id: Int): Option[FuturesMarket] = provider.futuresMarket(id)

  private def fromNameEither(marketName : String) : Either[Failure, CommodityMarket] = {
    val nameWithoutIdentifier = new Regex(" - [0-9]*$").replaceFirstIn(marketName, "")

    provider.allFuturesMarkets.find(_.name.equalsIgnoreCase(marketName)) match {
      case Some(market) => Right(market)
      case None => all.find(_.name.equalsIgnoreCase(nameWithoutIdentifier)) match {
        case Some(market) => Right(market)
        case None => Left(Failure("No known market with name '" + marketName + "' or '" + nameWithoutIdentifier + "'"))
      }
    }
  }

  private def fromCommodityEither(exchangeName: String, commodityName: String) = futuresMarkets.find(market =>
    market.exchange.name.equalsIgnoreCase(exchangeName) && market.commodity.name.equalsIgnoreCase(commodityName))
      .toRight(Failure("Exchange: %s does not have a market for commodity: %s" % (exchangeName, commodityName)))

  def fromNameOrCommodity(exchangeName: String, name: String): CommodityMarket =
    fromNameEither(name).orElse(fromCommodityEither(exchangeName, name)).getOrThrow

  def fromExchangeAndLimSymbol(exchange: String, limSymbol: String) =
    Market.futuresMarkets.find(market => market.exchange.name == exchange && market.limSymbol.map(_.name) == Some(limSymbol))

  /**
   * For testing
   */

  def testMarket(name : String, currency : UOM, uom : UOM) : FuturesMarket = {
    testMarket(name, currency, uom, -1, Day)
  }
  def testMarket(name : String, currency : UOM, uom : UOM, tenor: TenorType) : FuturesMarket = {
    testMarket(name, currency, uom, -1, tenor)
  }
  def testMarket(name : String, currency : UOM, uom : UOM, id : Int, tenor: TenorType) : FuturesMarket = {
    new FuturesMarket(name, Some(1.0), uom, currency, NilCalendar, Some(id), tenor, new FuturesExpiryRule {
      val name = "Test"

      def lastTradingDay(d: DateRange) = d.firstDay - 1
      override def expiryDay(d: DateRange) = d.firstDay - 1
    }, FuturesExchangeFactory.COMEX, Brent, (Conversions.default + ((BBL/MT, 7.57))))
  }
   def testMarketWithInterpolation(name : String, currency : UOM, uom : UOM) : FuturesMarket = {
    new FuturesMarket(name, Some(1.0), uom, currency, NilCalendar, None, Day, new FuturesExpiryRule {
      val name = "Test"
      def lastTradingDay(d: DateRange) = d.firstDay - 1
    }, FuturesExchangeFactory.COMEX, Brent, (Conversions.default + ((BBL/MT, 7.57)))) with HasInterpolation {
      def interpolation = LinearInterpolation
    }
  }
}

object FuturesMarket {
  def apply(
        name: String,
         lotSize: Option[Double],
         uom: UOM,
         currency: UOM,
         businessCalendar: BusinessCalendar,
         eaiQuoteID: Option[Int],
         tenor: TenorType,
         expiryRule: FuturesExpiryRule,
         exchange: FuturesExchange,
         commodity : Commodity,
         conversions: Conversions = Conversions.default,
         volatilityID : Option[Int] = None,
         limSymbol: Option[LimSymbol] = None,
         precision: Option[Precision] = None) = exchange match {
    case FuturesExchangeFactory.LME => new LMEFuturesMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, tenor, expiryRule, exchange, commodity, conversions, volatilityID, limSymbol, precision)
    case _ => new FuturesMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, tenor, expiryRule, exchange, commodity, conversions, volatilityID, limSymbol, precision)
  }
}

