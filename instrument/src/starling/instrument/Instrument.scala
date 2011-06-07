package starling.instrument


import starling.quantity.{Quantity, UOM}
import starling.quantity.UOM.{USD, BBL}
import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.Quantity._
import starling.curves._
import interestrate.{DayCountActual365, DayCountActualActual}
import starling.market._
import rules.SwapPricingRule
import starling.daterange._
import starling.varcalculator._
import starling.utils.ImplicitConversions._
import starling.models.DefaultRiskParameters
import starling.utils.CollectionUtils

trait AsUtpPortfolio {
  def asUtpPortfolio(tradeDay:Day): UTP_Portfolio
}

/**
 *  This class allows the reading of instruments from Trinity or TrinityArchive
 */
trait InstrumentType[T <: UTP] {
  val name : String

  override def toString = name
}

/**
 * An enumeration of all current trade types
 */
object InstrumentType {

  val types = List[InstrumentType[_ <: UTP]](
    Future,
    CommoditySwap,
    CFD,
    SwapCalendarSpread,
    FuturesOption,
    CalendarSpreadOption,
    ForwardOption,
    AsianOption,
    CommodityForward,
    FXOption,
    ErrorInstrument,
    CashInstrument,
    BankAccount,
    RefinedAssignment,
    RefinedFixation,
    NetEquityPosition,
    FuturesCalendarSpread,
    FuturesCommoditySpread
  )

  def fromName(name : String) = types.find(_.name.toLowerCase == name.toLowerCase) match {
    case Some(t) => t
    case None => throw new Exception("Couldn't find instrument with name " + name)
  }

  //the union of the keys in the Instrument#details method
  val fieldsWithType = List(
    ("Market", classOf[String]),
    ("Exercise Day",classOf[Day]),
    ("Strike",classOf[Quantity]),
//    ("Spread", classOf[Quantity]),
    ("Cleared", classOf[Boolean]),
    ("PricingRule", classOf[SwapPricingRule]),
    ("Quantity", classOf[Quantity]),
    ("Period", classOf[DateRange]),
//    ("Settlement Date", classOf[Day]),
    ("Delivery Day", classOf[Day]),
    ("Amount", classOf[Quantity]),
    ("Maturity Day", classOf[Day]),
    ("Call Put", classOf[String]),
    ("Exercise Type", classOf[String]),
    ("RIC", classOf[String]),
    ("Error", classOf[String]),
    ("Estimated Delivery", classOf[Day]),
    ("Fixation Date", classOf[Day]),
    ("Is Average Fixation", classOf[String]),
    ("Cash Instrument Type", classOf[String])
  )
  val fields = fieldsWithType.map(_._1)
  val lowercaseNoSpaceFields = fieldsWithType.map(_._1.toLowerCase.replaceAll(" ", ""))
}

/** The supertype of all instruments, including trades, which in turn contain instruments.
 */
trait Instrument extends Ordered[Instrument] with Greeks with PnlExplanation {

  def assets(env:Environment):Assets
  
  /** the MTM value of the given instrument using the environment curve data
  */
  def mtm(env : Environment):Quantity = assets(env).mtm(env, valuationCCY)

  /** to get MTM in another currency we multiply by the appropriate spot fx rate
  */
  def mtm(env : Environment, ccy : UOM) : Quantity = assets(env).mtm(env, ccy)

  /** The currency this instrument values in
   */
	def valuationCCY : UOM
 
	/** The atomic market data to which this instrument is sensitive.
   *
   * Warning: This will return things like
   * OilAtmVolAtomicDatumKey(Nymex WTI,MAY 2011,false)
   * OilAtmVolAtomicDatumKey(Nymex WTI,MAY 2011,true)
   * i.e. the only difference is the boolean `ignoreShiftsIfPermitted`
   * @see AtomicDatumKeyUtils for a distinct version
  */
  def atomicMarketDataKeys(marketDay: DayAndTime, ccy : UOM = UOM.USD): Set[AtomicDatumKey] = {
    val record = KeyRecordingCurveObjectEnvironment(new NullAtomicEnvironment(marketDay))
    mtm(Environment(record), ccy)
    record.keys.map(_.clearProperties)
  }

  def environmentDifferentiables(marketDay: DayAndTime, ccy : UOM = UOM.USD): Set[EnvironmentDifferentiable] = {
    atomicMarketDataKeys(marketDay, ccy).flatMap(EnvironmentDifferentiable.toEnvironmentDifferentiable)
  }

  def pricePeriods(marketDay : DayAndTime) : Map[CommodityMarket, DateRange] = {
    CollectionUtils.filterOnType[ForwardPriceKey](atomicMarketDataKeys(marketDay)).toList.groupBy(_.market).map{
      case (market, keys) =>
        val periods = keys.map(_.period).sortWith(_<_)
        market -> SimpleDateRange(periods.head.firstDay, periods.last.lastDay)
    }.toMap
  }

  /** This ordering is intended as for convenience only, especially during unit tests where it can be useful
   *  to compare different outputs in the same order 
   */
  def compare(rhs : Instrument) : Int = {
    this.toString.compare(rhs.toString)
  }

  /** Returns the relevant VaR risk factors as of the given market day, when the deal is
   * valued in the given currency
   * <p>
   */
   //TODO [21 Oct 2009] This kind of sucks - either
   //TODO [21 Oct 2009]       a) Lose atomicMarketDataKeys and put this method in the Instrument sub classes
   //TODO [21 Oct 2009]       b) pass the request to the key itself
  def riskFactors(env: Environment, ccy : UOM) : Set[RiskFactor] = {
    val marketDayAndTime : DayAndTime = env.marketDay
    val marketDay: Day = marketDayAndTime.day

    val rfs= atomicMarketDataKeys(env.marketDay).flatMap{
      key => key match {
        case ForwardPriceKey(market, dateRange, _) => market.priceRiskFactor(marketDayAndTime, dateRange)
        case USDFXRateKey(currency) if currency != ccy => Some(SpotFXRiskFactor(currency))
        case OilAtmVolAtomicDatumKey(market, _, period, _) => Some(VolatilityRiskFactor(market, period))
        case EquityPriceKey(ric) => Some(EquityRiskFactor(ric))
        case BradyMetalVolAtomicDatumKey(market, period) => Some(VolatilityRiskFactor(market, period))

        // Put this back when we find a decent source of historic interest rates
//        case DiscountRateKey(currency, day) => {
//          val firstDay = marketDayAndTime.timeOfDay match {
//            case TimeOfDay.EndOfDay => marketDay + 1
//            case TimeOfDay.StartOfDay => marketDay
//          }
//          if (day > firstDay)
//            Some(ForwardRateRiskFactor(currency, firstDay - marketDay, day - marketDay))
//          else
//            None
//        }
        
        case _ => None
      }
    }
    Set.empty ++ rfs
  }

  def varRiskFactors(env : Environment, ccy : UOM) : Set[VaRRiskFactor] = riskFactors(env, ccy).flatMap{
    case vrf : VaRRiskFactor => Some(vrf)
    case _ => None
  }
  /**
   * An instrument may have many risk factors wrt the same market/type - e.g. daily
   * LME Lead price risk factors. This merges these so that each market/type
   * is represented by a single risk factor
   */
  private def mergedVaRRiskFactors(env: Environment, valCCY : UOM) : Set[VaRRiskFactor] = {
    var set = Set[VaRRiskFactor]()
    varRiskFactors(env, valCCY).foreach{
      rf => set.find{case rf_ => rf_.riskFactorType == rf.riskFactorType} match {
        case Some(rf_) => {
          set = set - rf_
          set = set + (rf merge rf_)
        }
        case None => {
          set = set + rf
        }
      }
    }
    set
  }
  
  def riskFactorDerivative(envs : ShiftedEnvironments, valuationCurrency : UOM) : Quantity = {
    val mtmUp = mtm(envs.upEnv, valuationCurrency)
    val mtmDown = mtm(envs.downEnv, valuationCurrency)
    (mtmUp - mtmDown) / (envs.dP *  2.0)
  }

  def riskFactorDerivative(env : Environment, rf : RiskFactor, valuationCurrency : UOM) : Quantity = {
    riskFactorDerivative(rf.shiftedEnvironments(env), valuationCurrency)
  }

  def riskFactorPosition(envs : ShiftedEnvironments, rf : RiskFactor, valCCY : UOM) : Quantity = {
    val delta = riskFactorDerivative(envs, valCCY)
    if(delta.isZero) {
      0 (rf.riskFactorType.positionUOM)
    } else {
      riskFactorPosition(envs, rf, delta, valCCY)
    }
  }

  /**
   * calculate position, or volume of standard hedging instrument. Kind of sucks that
   * the matching is done here, but EnvironmentDifferentiables know nothing of instruments.
   * For EnvironmentDifferentiables that have no standard hedge (e.g. vols) we simple
   * return the derivative
   */
  def position(env : Environment, diff : EnvironmentDifferentiable) : Quantity = {
    val hedge = hedgingInstrument(env, diff)
    val delta = firstOrderDerivative(env, diff, USD)
    val hedgeDelta = hedge match{
      case Some(inst) => inst.firstOrderDerivative(env, diff, USD)
      case None => 1.0(delta.uom)
    }
    val hedgeRatio = if (delta.isZero && hedgeDelta.isZero) 0.0 else (delta / hedgeDelta).checkedValue(UOM.SCALAR)
    hedgeRatio (delta.uom)
  }

  def hedgingInstrument(env: Environment, diff: EnvironmentDifferentiable): Option[UTP] = {
    def buildSwap(index : SingleIndex, period : DateRange) : Option[SingleCommoditySwap] = {
        // we have to be careful to take into account when we are mid-pricing period.

        val livePeriodDays = index.observationDays(period).filter(_.endOfDay > env.marketDay)
        if (!livePeriodDays.isEmpty) {
          Some(SingleCommoditySwap(index, 0.0(index.priceUOM), 1.0(index.uom), DateRange(livePeriodDays.head, livePeriodDays.last), cleared = true))
        } else {
          None
        }
    }
    val hedge = diff match {
      case PriceDifferentiable(market: FuturesMarket, period) => Some(Future(market, period, 0.0(market.priceUOM), 1.0(market.uom)))
      case PriceDifferentiable(market : CommodityMarket, period) if PublishedIndex.marketToPublishedIndexMap.contains(market) => {
        buildSwap(PublishedIndex.marketToPublishedIndexMap(market), period)
      }
      case PriceDifferentiable(market: ForwardMarket, day: Day) => Some(CommodityForward(market, day, 0.0(market.priceUOM), 1.0(market.uom)))
      // This sucks but should be gotten rid of once ForwardMarket goes
      case PriceDifferentiable(market: ForwardMarket, period : DateRange) => {
        val days = period.days.filter(market.businessCalendar.isBusinessDay)
        Some(CommodityForward(market, days.last, 0.0(market.priceUOM), 1.0(market.uom)))
    }
      case FuturesSpreadPrice(market, m1, m2) => Some(FuturesCalendarSpread(market, m1, m2, 0.0(market.priceUOM), 0.0(market.priceUOM), 1.0(market.uom)))
      case SwapPrice(index, period) => {
        buildSwap(index, period)
      }
      case _ => None
    }
    hedge
  }

  def riskFactorPosition(envs : ShiftedEnvironments, rf : RiskFactor, delta: Quantity, valCCY : UOM) : Quantity = {
    val env = envs.env
    val marketDay = env.marketDay.day
    val hedgingInstrument:HedgingTradeable = rf match {
      case ForwardPriceRiskFactor(market, nDaysToStart, nDaysToEnd)
      => market match {
        case f: FuturesMarket => Future(f, f.nthPeriod(env.marketDay, nDaysToEnd), 0(market.priceUOM), 1(market.uom))
        case f: ForwardMarket => CommodityForward(f, f.nthPeriod(env.marketDay, nDaysToEnd), 0(market.priceUOM), 1(market.uom))
      }
      case EquityRiskFactor(ric) => NetEquityPosition(ric, Quantity(1, UOM.SHARE))
      case SpotFXRiskFactor(ccy) => new FXForward(env.spotFXRate(USD, ccy), 1.0 (ccy), marketDay)
    }

    val hedgeDelta = hedgingInstrument.asUtpPortfolio().riskFactorDerivative(envs, valCCY)
    val hedgeRatio = if (delta.isZero && hedgeDelta.isZero) 0.0 else (delta / hedgeDelta).checkedValue(UOM.SCALAR)
    hedgeRatio (rf.riskFactorType.positionUOM)
  }
  def riskFactorPosition(env : Environment, rf : RiskFactor, valCCY : UOM) : Quantity = {
    riskFactorPosition(rf.shiftedEnvironments(env), rf, valCCY)
  }

  /**
   * Total position wrt to some risk factor type, i.e. parallel shift delta wrt WTI prices,
   * or parallel shift vega wrt WTI vols
   */
  // TODO [20 May 2010] distinguish between Vol and Prices - are they the same RiskFactorMarket?
  // TODO [20 May 2010] ensure maximalRiskFactors returns a single risk factor per market/risk type
  def riskFactorTypePosition(env : Environment, rfType : RiskFactorType, valCCY : UOM) : Quantity = {
    mergedVaRRiskFactors(env, valCCY).find(_.riskFactorType == rfType) match {
      case Some(rf) => riskFactorPosition(env, rf, valCCY)
      case None => 0 (rfType.positionUOM)
    }
  }

  /**
   * Returns a map of market -> parallel shift position wrt that market
   */
  def parallelShiftPositions(env : Environment) : MarketPositions = {
    (new MarketPositions /: mergedVaRRiskFactors(env, USD).map{ rf => rf.riskFactorType -> riskFactorPosition(env, rf, USD)}) (_+_)
  }

  def usdDeltaPosition(env : Environment, riskFactor : VaRRiskFactor) : Quantity = {
    usdDeltaPosition(env, riskFactor, riskFactorPosition(env, riskFactor, USD))
  }

  def usdDeltaPosition(env : Environment, riskFactor : VaRRiskFactor, delta: Quantity) : Quantity = {
    val naturalCurrencyDeltaPosition = delta * riskFactor.price(env)
    val spotFXRate = env.spotFXRate(USD, naturalCurrencyDeltaPosition.uom)
    naturalCurrencyDeltaPosition * spotFXRate
  }

  def oilBarrelPosition(env: Environment, rf: RiskFactor): Quantity = {
    val position = riskFactorPosition(env, rf, valuationCCY)
    oilBarrelPosition(rf, position)
  }

  def oilBarrelPosition(rf: RiskFactor, position: Quantity): Quantity = {
    rf match {
      case ForwardPriceRiskFactor(market, _, _) if market.commodity.isInstanceOf[OilCommodity] =>
        market.convertUOM(position, BBL)
      case _ => Quantity.NULL
    }
  }
  def oilBarrelPosition(env: Environment): Quantity = {
    Quantity.sum(mergedVaRRiskFactors(env, valuationCCY).toList.map(oilBarrelPosition(env, _)))
  }

  def commodityFuturesPosition(env : Environment, rf : RiskFactor) : Quantity = {
    val position = riskFactorPosition(env, rf, valuationCCY)
    commodityFuturesPosition(rf, position)
  }

  def commodityFuturesPosition(rf: RiskFactor, position: Quantity): Quantity = {
    rf match {
      case prf : ForwardPriceRiskFactor => {
         prf.market.commodity.toStandardFuturesLots(position)
      }
      case _ => Quantity.NULL
    }
  }

  /**
   *  Calculates the sum of delta * price, converted to USD, for all the risk factors of this instrument
   * Used to produce 'signed' VaR for Lawrence, who wants to see VaR with the same sign as this number,
   * rather than always being negative
   */
  def usdDeltaPosition(env : Environment) : Quantity = {
    Quantity.sum(varRiskFactors(env, USD).toList.map(usdDeltaPosition(env, _)))
  }

  def instrumentVolume : Quantity = {
    throw new UnsupportedOperationException
  }
  /**
   * This sucks a little. JF would like to see the skew adjusted vol for a UTP in pivot reports. Not
   * unreasonable. We initially displayed the ATM vol. We pass in the ATM vol key, from this the UTP
   * must determine which interpolated vol to get. 
   */
  def interpolatedVol(env : Environment, volKey : EnvironmentDifferentiable with VolKey) : Quantity = {
    throw new UnsupportedOperationException(this + " does not know how to calculate interpolatedVol")
  }

  /**
   * Some instruments round when working out the MTM which makes the mtm non-differentiable and the pnl
   * explanation have rounding errors.
   *
   * By specifying the rounding used the pnl explanation can at least bucket the explanation
   * difference in the correct place.
   */
  def priceRounding: Option[Int] = None

  /**
   * True if this instrument is a linear instrument on this market day. So it will return true
   * for an expired option, for example. 
   */
  def isLinear(marketDay: DayAndTime) = !environmentDifferentiables(marketDay).exists(_.isInstanceOf[VolKey])
}


case class CompositeInstrument(insts : Seq[Instrument]) extends Instrument {

  def valuationCCY = {
    val ccys : Set[UOM] = Set.empty ++ insts.map(_.valuationCCY)
    if (ccys.size == 1)
      ccys.iterator.next
    else
      throw new IllegalStateException("Composite has a mixture of currencies")
  }

  def assets(env: Environment) = Assets(insts.flatMap(_.assets(env).assets).toList)
}
