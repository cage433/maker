package starling.instrument


import physical.{Cargo, PhysicalMetalAssignment}
import starling.quantity.UOM.{USD, BBL}
import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.Quantity._
import starling.curves._
import interestrate.{DayCountActual365, DayCountActualActual}
import starling.market._
import rules.{RoundingMethodRule, SwapPricingRule}
import starling.daterange._
import starling.varcalculator._
import starling.utils.ImplicitConversions._
import starling.models.DefaultRiskParameters
import starling.utils.CollectionUtils
import starling.marketdata.ReferenceDataLookup
import starling.quantity.{SimpleNamedQuantity, NamedQuantity, Quantity, UOM}

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
    TAS,
    FFA,
    CommoditySwap,
    SwapCalendarSpread,
    FuturesOption,
    CalendarSpreadOption,
    CommoditySpreadOption,
    AsianOption,
    ErrorInstrument,
    CashInstrument,
    BankAccount,
    RefinedAssignment,
    RefinedFixation,
    FuturesCalendarSpread,
    FuturesCommoditySpread,
    PhysicalMetalAssignment
  )

  def fromName(name : String) = types.find(_.name.toLowerCase == name.toLowerCase)
}
/** The supertype of all instruments, including trades, which in turn contain instruments.
 */
trait Instrument extends Ordered[Instrument] with Greeks with PnlExplanation {

  def assets(env:Environment):Assets
  def assets(env:Environment, ccy : UOM):Assets = Assets(assets(env).assets.map(_.inCCY(env, ccy)))

  // Return a tree structure describing how mtm was calculated
  def explain(env: Environment, ccy: UOM): NamedQuantity = {
    val explained = explanation(env) in ccy match {
      case Some(ex) => ex
      case None => {
        val spotFX = env.withNaming().spotFXRate(ccy, valuationCCY) match {
          case nq : SimpleNamedQuantity => nq
          case q => q.named("Spot FX")  // Only wrap with 'Spot FX' name if it is calculated, i.e. a product or inversion
        }
        (explanation(env) * spotFX) inUOM ccy
      }
    }

    assert(explained.isAlmostEqual(mtm(env, ccy), 1e-6), "Explanation not the same as the mtm: " + (explained, mtm(env, ccy)))
    explained
  }

  def explain(env: Environment): NamedQuantity = explain(env, USD)

  /**
   * Explains the valuation of this instrument.
   *
   * Not to be called directly, that's why it's protected, call 'explain' above.
   */
  protected def explanation(env: Environment): NamedQuantity

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
  def atomicMarketDataKeys(env : Environment, ccy : UOM = UOM.USD): Set[AtomicDatumKey] = {
    val record = KeyRecordingCurveObjectEnvironment(env.atomicEnv)
    mtm(Environment(record), ccy)
    record.keys.map(_.clearProperties)
  }

  def environmentDifferentiables(env : Environment, ccy : UOM = UOM.USD): Set[EnvironmentDifferentiable] = {
    atomicMarketDataKeys(env, ccy).flatMap(EnvironmentDifferentiable.toEnvironmentDifferentiable)
  }

  def pricePeriods(env : Environment) : Map[CommodityMarket, DateRange] = {
    CollectionUtils.filterOnType[ForwardPriceKey](atomicMarketDataKeys(env)).toList.groupBy(_.market).map{
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


  /**
   * calculate position, or volume of standard hedging instrument. Kind of sucks that
   * the matching is done here, but EnvironmentDifferentiables know nothing of instruments.
   * For EnvironmentDifferentiables that have no standard hedge (e.g. vols) we simple
   * return the derivative
   */
  def position(env : Environment, diff : EnvironmentDifferentiable with PriceKey) : Quantity = {
    val hedge = hedgingInstrument(env, diff)
    val delta = firstOrderDerivative(env, diff, valuationCCY)
    val hedgeDelta = hedge match{
      case Some(inst) => inst.firstOrderDerivative(env, diff, valuationCCY)
      case None => 1.0(delta.uom)
    }
    val hedgeRatio = if (delta.isZero && hedgeDelta.isZero) 0.0 else (delta / hedgeDelta).checkedValue(UOM.SCALAR)
    hedgeRatio (diff.market.uom)
  }

  def hedgingInstrument(env: Environment, diff: EnvironmentDifferentiable): Option[UTP] = {
    def buildSwap(index : SingleIndex, period : DateRange) : Option[SinglePeriodSwap] = {
        // we have to be careful to take into account when we are mid-pricing period.

        val livePeriodDays = index.observationDays(period).filter(_.endOfDay > env.marketDay)
        if (!livePeriodDays.isEmpty) {
          Some(SinglePeriodSwap(index, 0.0(index.priceUOM), 1.0(index.uom), DateRange(livePeriodDays.head, livePeriodDays.last), cleared = true))
        } else {
          None
        }
    }
    val hedge = diff match {
      case PriceDifferentiable(market: FuturesMarket, period) => {
        val futuresPeriod = if (market.tenor == Day) market.observationDays(period).last else period
        Some(Future(market, futuresPeriod, 0.0(market.priceUOM), 1.0(market.uom)))
      }
      case PriceDifferentiable(market : CommodityMarket, period) if Index.marketToPublishedIndexMap.contains(market) => {
        buildSwap(Index.marketToPublishedIndexMap(market), period)
      }
      case FuturesSpreadPrice(market, SpreadPeriod(m1: Month, m2: Month)) => Some(FuturesCalendarSpread(market, m1, m2, 0.0(market.priceUOM), 0.0(market.priceUOM), 1.0(market.uom)))
      case SwapPrice(index, period) => {
        buildSwap(index, period)
      }
      case _ => None
    }
    hedge
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
  def isLinear(env: Environment) = !environmentDifferentiables(env).exists(_.isInstanceOf[VolKey])
}


case class CompositeInstrument(insts : Seq[Instrument]) extends Instrument {

  def explanation(env: Environment) = Quantity.sumNamed(insts.map(_.explain(env)))

  def valuationCCY = {
    val ccys : Set[UOM] = Set.empty ++ insts.map(_.valuationCCY)
    if (ccys.size == 1)
      ccys.iterator.next
    else
      throw new IllegalStateException("Composite has a mixture of currencies")
  }

  def assets(env: Environment) = Assets(insts.flatMap(_.assets(env).assets).toList)
}
