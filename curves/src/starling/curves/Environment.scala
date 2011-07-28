package starling.curves

import interestrate.{DayCountActual365, DayCount}
import models.SpreadVolatilitySkew
import starling.market._
import rules.{Precision, SwapPricingRule}
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.maths._
import starling.models._
import math.log
import cern.colt.matrix.DoubleMatrix2D
import starling.daterange._
import starling.market.formula.FormulaIndex
import starling.utils.{MathUtil, Log}
import scala.math._
import starling.utils.cache.{SimpleCache, CacheFactory}
import stress.CommodityPriceStress
import starling.quantity.{Conversions, Percentage, Quantity, UOM}

/**
 * Throw this if a curve object is incapable of providing a value
 * because of a lack of market data only
 */

class MissingMarketDataException(s : String = "Missing data", cause : Throwable = null) extends RuntimeException(s, cause)
class MissingPriceException(val marketName: String, val period: DateRange, msg : => String) extends MissingMarketDataException(msg)
class MissingForwardFXException(val ccy: UOM, val forwardDay: Day, msg : => String) extends MissingMarketDataException(msg)

/** <p>
 * 		This class provides access to a snapshot of market data
 * 	</p>
 * 	<p>
 * 		The Environment class is in three parts. At its core is an AtomicEnvironment. This is the
 * 		source from which atomic market data is obtained. By 'atomic' market data, we mean those
 * 		data which are independent, in an arbitrage sense, from all others. 'Composite' market 
 * 		data can be expressed, via arbitrage, as a function of other data, either atomic or
 * 		composite. Examples of composite data are forward fx rates and frontline swap rates.
 *    Perturbations to atomic data can be done freely without the risk of inducing arbitrage.
 * 	</p>
 * 	<p>
 *    Between this level and the AtomicEnvironment lies the 'InstrumentLevelEnvironment'. This exists
 *    so that (not necessarily arbitrage free) perturbations can be performed. These are required for
 *    various risk reports. The example that lead to this three-tier approach was the necessity to
 *    differentiate wrt the expected value of a fixing on some observation day, while leaving the
 *    underlying unchanged. Without this it is difficult to break swap positions into natural looking
 *    daily positions.
 * 	</p>
 * 	<p>
 *    The highest level, Environment, handles caching, building perturbations, and also has some
 *    convenience functions, e.g. 'averaheImpliedVol'
 * 	</p>
 * 	@see AtomicEnvironment
 */
case class Environment(
  instrumentLevelEnv : InstrumentLevelEnvironment,
  environmentParameters : EnvironmentParameters = DefaultValuationParameters
)
{
  // Parameterless constructor for ThreadSafeCachingProxy
  def this() = this(null)

  def atomicEnv = instrumentLevelEnv.atomicEnv
  def named(prefix : String = "") = Environment(NamingAtomicEnvironment(atomicEnv, prefix))

  // cache for optimization of greek calculations. Will cache shifted environments, forward state envs and mtms
  // Level of indirection so that InstrumentLevelPerturbedEnvironment can avoid using the same cache as the
  // object it overrides
  private val greeksCacheInstance = CacheFactory.getCache("Environment.greeks", unique = true)
  def greeksCache = greeksCacheInstance

  /** The day and also time of day (start or end) of the market data snapshot
   */
  def marketDay : DayAndTime = instrumentLevelEnv.marketDay

  def shiftsCanBeIgnored = atomicEnv.shiftsCanBeIgnored
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) : Environment = copy(instrumentLevelEnv = instrumentLevelEnv.setShiftsCanBeIgnored(canBeIgnored))

  /**
   * Used by some of the tests, it means that averagePrice doesn't use the rounding rules of the market
   */
  def ignoreSwapRounding = copy(environmentParameters = environmentParameters.copy(swapRoundingOK = false))

  /**	Returns the current spot fx rate in units ccy1 / ccy2
   */
  def spotFXRate(ccy1: UOM, ccy2: UOM): Quantity = {
    if (!(ccy1.isCurrency && ccy2.isCurrency)){
      println("Not currency")
    }
    assert(ccy1.isCurrency, ccy1 + " is not a currency")
    assert(ccy2.isCurrency, ccy2 + " is not a currency")
    (ccy1, ccy2) match {
      case (ccy1, ccy2) if ccy1 == ccy2 => new Quantity(1.0)
      case (USD, US_CENT) => (1/100.0) (USD/US_CENT)
      case (`USD`, ccy) => instrumentLevelEnv.quantity(USDFXRateKey(ccy))
      case (ccy, `USD`) => spotFXRate(USD, ccy).invert
      case _ => spotFXRate(ccy1, USD) * spotFXRate(USD, ccy2)
    }
  }

  def forwardFXRate(fxUnit : UOM, maturityDate : Day) : Quantity = {
    forwardFXRate(fxUnit.numeratorUOM, fxUnit.denominatorUOM, maturityDate)
  }

  def equityPrice(ric: RIC) = instrumentLevelEnv.quantity(EquityPriceKey(ric)) 

  def forwardFXRate(ccy1 : UOM, ccy2 : UOM, maturityDate : Day) : Quantity = {
    if (ccy1 == ccy2)
      new Quantity(1.0)
    else {
      val spotRate = spotFXRate(ccy1, ccy2)
      val d1 = discount(ccy1, maturityDate)
      val d2 = discount(ccy2, maturityDate)

      spotRate * d2 / d1
    }
  }

  def spreadPrice(market: FuturesMarket, period: Period) = instrumentLevelEnv.spreadPrice(market, period: Period)

  /** Returns the futures/forward price for the given market and forward date
   */
  def forwardPrice(market: CommodityMarket, forwardDate : DateRange) : Quantity = {
    instrumentLevelEnv.quantity(ForwardPriceKey(market, forwardDate))
  }

  def forwardPrice(market: CommodityMarket, forwardDate : DateRange, unit: UOM) : Quantity = {
    (forwardPrice(market, forwardDate) * spotFXRate(unit.numeratorUOM, market.currency) in unit).getOrElse(
      throw new Exception("Can't convert " + market + " price (" + market.priceUOM + ") to " + unit))
  }

  private var averagePriceCache = CacheFactory.getCache("Environment.averagePrice", unique = true)

  def averagePrice(index: SingleIndex, averagingPeriod: DateRange): Quantity = averagePrice(index, averagingPeriod, None)

  def averagePrice(index: SingleIndex, averagingPeriod: DateRange, rounding: Option[Int]): Quantity = averagePriceCache.memoize(
    (index, averagingPeriod),
    (tuple: (Index, DateRange)) => {
      val observationDays = index.observationDays(averagingPeriod)
      val price = Quantity.average(observationDays.map(fixingOrForwardPrice(index, _)))
      rounding match {
        case Some(dp) if environmentParameters.swapRoundingOK => {
          price.round(dp)
        }
        case _ => price
      }
    }
   )

   def fixingOrForwardPrice(index : SingleIndex, observationDay : Day) = {
     val price = if (observationDay.endOfDay <= marketDay) {
       indexFixing(index, observationDay)
     } else {
       indexForwardPrice(index, observationDay, ignoreShiftsIfPermitted = false)
     }
     price
  }


  /**
   * Average price for the period.
   *
   * @param rule The pricing rule used if the index is a multi-index. Needed to know if pricing is common or non-common
   * @param priceUOM The UOM to convert to (if possible). A lot of swaps are booked against a uom other than their market
   * uom or against multi-indexes so need the conversion. The conversion is very simple for the moment, it converts each
   * price independently, using its own market, into the requested priceUOM.
   */
  def averagePrice(index: Index, averagingPeriod: DateRange, rule: SwapPricingRule, priceUOM: UOM, rounding: Option[Int] = None): Quantity = {
    val value = index match {
      case si: SingleIndex => {
        val price = averagePrice(si, averagingPeriod)
        index.convert(price, priceUOM) match {
          case Some(p) => p
          case None => throw new Exception(this + ": Couldn't convert from " + price.uom + " to " + priceUOM + " with " + index)
        }
      }
      case mi: MultiIndex => {
        averagePriceCache.memoize((index, averagingPeriod, rule, priceUOM), mi.averagePrice(this, averagingPeriod, rule, priceUOM))
      }
    }
    rounding match {
      case Some(dp) if environmentParameters.swapRoundingOK => {
        value.round(dp)
      }
      case _ => value
    }
  }

    /**	Returns the discount rate for the given currency and date
   */
  def discount(ccy : UOM, forwardDate : Day) : Quantity = {
    instrumentLevelEnv.discount(ccy, forwardDate)
  }

  /**Returns a forward rate using the appropriate daycount convention.
   * default daycount = DayCountActual365
   */
  def forwardRate(ccy : UOM, day1 : Day, day2 : Day, dayCount : DayCount= DayCountActual365) : Percentage = {
    assert(day1 <= day2, "Forward rate day2 " + day2 + " is before day1 " + day1)
    val d1 = discount(ccy, day1)
    val d2 = discount(ccy, day2)
    val T = dayCount.factor(day1, day2)
    if (T == 0.0)
      Percentage(0.0)
    else
      Percentage((d1 / d2 - 1.0) / T)
  }

  def zeroRate(currency: UOM, day: Day): Percentage = {
    assert(day >= marketDay.day, "Can't get zero rate before market day")
    if (day == marketDay.day)
      Percentage(0.0)
    else {
      val disc = discount(currency, day)
      val t = DayCountActual365.factor(marketDay.day, day)
      val zero = -log(disc.checkedValue(UOM.SCALAR)) / t
      Percentage(zero)
    }
  }

  def forwardRate(ccy : UOM, period : DateRange, dayCount : DayCount) : Percentage =
    forwardRate(ccy, period.firstDay, period.lastDay, dayCount)

  def forwardCCRate(ccy : UOM, day1 : Day, day2 : Day, dayCount : DayCount) = {
    assert(day1 <= day2, "Forward rate day2 " + day2 + " is before day1 " + day1)
    val d1 = discount(ccy, day1)
    val d2 = discount(ccy, day2)
    val T = dayCount.factor(day1, day2)
    if (T == 0.0)
      Percentage(0.0)
    else
      Percentage(log((d1 / d2).checkedValue(UOM.SCALAR)) / T)
  }

  def indexFixing(index : SingleIndex, fixingDay : Day) : Quantity = {
    instrumentLevelEnv.fixing(index, fixingDay)
  }

  def indexForwardPrice(index : SingleIndex, observationDay : Day, ignoreShiftsIfPermitted : Boolean = false) : Quantity = {
    instrumentLevelEnv.indexForwardPrice(index, observationDay, ignoreShiftsIfPermitted)
  }


  /**
   * Vols
   */

  private var impliedVolHelperCache = CacheFactory.getCache("Environment.impliedVolHelper", unique = true)

  def swapVol(index: SingleIndex, averagingPeriod: DateRange, strike: Quantity): Percentage = {
    val unfixedDays = index.observationDays(averagingPeriod).filter(_.endOfDay > marketDay)

    val averageUnfixedPrice = unfixedDays.map{d => instrumentLevelEnv.indexForwardPrice(index, d, ignoreShiftsIfPermitted = true)}.sum / unfixedDays.size
    Percentage.average(unfixedDays.map(instrumentLevelEnv.indexVol(index, _, strike, averageUnfixedPrice)))
  }

  def indexVol(index: SingleIndex, observationDay: Day, strike: Quantity, averagePrice: Quantity): Percentage = {
    instrumentLevelEnv.indexVol(index, observationDay, strike, averagePrice)
  }

  def impliedVol(market : HasImpliedVol, period : DateRange, exerciseDay : Day, strike : Quantity) : Percentage = {
    impliedVol(market, period, Some(exerciseDay), Some(strike))
  }
  def atmImpliedVol(market : HasImpliedVol, period : DateRange) : Percentage = {
    impliedVol(market, period, None, None)
  }

  /**
   * @param exerciseDay is an option, it is not used if strike is None, otherwise it must be present
   * @param strike is an option, if it's None the atm vol is returned
   */
  def impliedVol(market : HasImpliedVol, period : DateRange, exerciseDay : Option[Day], strike : Option[Quantity]) : Percentage = {
    instrumentLevelEnv.interpolatedVol(market, period, exerciseDay, strike, isIndexVol = false, None)
  }

  def spreadStdDev(market: FuturesMarket, period: Period, exerciseDay: Day, strike: Quantity) = {
    instrumentLevelEnv.spreadStdDev(market, period, exerciseDay, strike)
  }

  /**
   * Perturbations
   */
  private def applyAtomicShift(fn : AtomicEnvironment => AtomicEnvironment) = {
    val env = copy(instrumentLevelEnv = instrumentLevelEnv.shiftAtomicEnv(fn))
    env
  }

  /**
   * Price pertubations
   */
  def parallelShiftPrices(market: CommodityMarket, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftPricePar" + (market, dP), original, {
      case key@ForwardPriceKey(`market`, _, _) => original.quantity(key) + dP
    }))
  }

  def shiftPrice(market: CommodityMarket, period: DateRange, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftPrice" + (market, period, dP), original, {
      case key@ForwardPriceKey(`market`, dateRange, _) if period.contains(dateRange) => original.quantity(key) + dP
    }))
  }

  def parallelShiftPrices(commodity: Commodity, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftPricePar" + (commodity, dP), original, {
      case key@ForwardPriceKey(market, _, _) if market.commodity == commodity => original.quantity(key) + dP
    }))
  }

  def shiftPrice(commodity: Commodity, period: DateRange, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftPrice" + (commodity, period, dP), original, {
      case key@ForwardPriceKey(market, dateRange, _)
        if market.commodity == commodity && period.contains(dateRange) => original.quantity(key) + dP
    }))
  }

  def parallelStressShiftPrices(commodity: Commodity, dP: Quantity): Environment = {
    applyAtomicShift(new CommodityPriceStress(_, commodity, dP))
  }

  /**
   * Vol pertubations
   */
  def parallelShiftVols(market: HasImpliedVol, dP: Percentage): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftVolsPar" + (market, dP), original, {
      case key@BradyFXVolSmileAtomicDatumKey(`market`, _) => {
        original.percentage(key) + dP
      }
      case key@BradyMetalVolAtomicDatumKey(`market`, _) => {
        original.percentage(key) + dP
      }
      case key@OilAtmVolAtomicDatumKey(`market`, _, _, _) => {
        original.percentage(key) + dP
      }
    }))
  }

  def shiftVol(market: HasImpliedVol, shiftedObservationDays: Option[DateRange], period: DateRange, dP: Percentage): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftVols" + (market, shiftedObservationDays, period, dP), original, {
      case key@BradyFXVolSmileAtomicDatumKey(`market`, p) if period.contains(p) => {
        original.percentage(key) + dP
      }
      case key@BradyMetalVolAtomicDatumKey(`market`, p) if period.contains(p) => {
        original.percentage(key) + dP
      }
      case key@OilAtmVolAtomicDatumKey(`market`, observationDays, p, _) if period.contains(p) => {
        val isShiftedObservationDay = (shiftedObservationDays, observationDays) match {
          case (Some(dr1), Some(dr2)) => dr1.contains(dr2)
          case _ => true
        }
        if (isShiftedObservationDay)
          original.percentage(key) + dP
        else
          original.percentage(key)
      }
    }))
  }

  def parallelShiftVols(commodity: Commodity, dP: Percentage): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftVolsPar" + (commodity, dP), original, {
      case key@BradyMetalVolAtomicDatumKey(market, _) if market.commodity == commodity => {
        original.percentage(key) + dP
      }
      case key@OilAtmVolAtomicDatumKey(market, _, _, _) if market.commodity == commodity => {
        original.percentage(key) + dP
      }
    }))
  }

  def shiftVol(commodity: Commodity, period: DateRange, dP: Percentage): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftVols" + (commodity, period, dP), original, {
      case key@BradyMetalVolAtomicDatumKey(market, p) if period.contains(p) && market.commodity == commodity => {
        original.percentage(key) + dP
      }
      case key@OilAtmVolAtomicDatumKey(market, _, p, _)
        if period.contains(p) && market.commodity == commodity => {
        original.percentage(key) + dP
      }
    }))
  }


  /**
   * Std dev pertubations
   */
  def parallelShiftSpreadStdDevs(market: FuturesMarket, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftStdDevPar" + (market, dP), original, {
      case key@SpreadAtmStdDevAtomicDatumKey(`market`, _, _) => {
        original.quantity(key) + dP
      }
    }))
  }

  def shiftSpreadStdDevs(market: FuturesMarket, period: Period, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftStdDev" + (market, period, dP), original, {
      case key@SpreadAtmStdDevAtomicDatumKey(`market`, `period`, _) => {
        original.quantity(key) + dP
      }
    }))
  }

  def parallelShiftSpreadStdDevs(commodity: Commodity, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftStdDevPar" + (commodity, dP), original, {
      case key@SpreadAtmStdDevAtomicDatumKey(market, _, _) if market.commodity == commodity => {
        original.quantity(key) + dP
      }
    }))
  }

  def shiftSpreadStdDevs(commodity: Commodity, period: Period, dP: Quantity): Environment = {
    applyAtomicShift(original => GenericPerturbedAtomicEnvironment("shiftStdDev" + (commodity, period, dP), original, {
      case key@SpreadAtmStdDevAtomicDatumKey(market, `period`, _) if market.commodity == commodity => {
        original.quantity(key) + dP
      }
    }))
  }


  def shiftFwdFwdRate(ccy : UOM, period : DateRange, dR : Quantity) : Environment = applyAtomicShift(ShiftForwardForwardRate(_, ccy, period, dR))
  def undiscounted : Environment = applyAtomicShift(Undiscounted(_))
  def zeroVols : Environment = applyAtomicShift(ZeroVols(_))
  def shiftSpotFX(ccy : UOM, dP : Quantity) : Environment = applyAtomicShift(ShiftSpotFX(_, ccy, dP))
  def shiftEquityPrice(ric : RIC, dP : Quantity) : Environment = applyAtomicShift(ShiftEquityPrice(_, ric, dP))
  def forwardState(forwardDayAndTime : DayAndTime) : Environment = applyAtomicShift(ForwardStateEnvironment(_, forwardDayAndTime))
  def forwardDiscounting(forwardDay : Day) = applyAtomicShift(ForwardDiscountingEnvironment(_, forwardDay))

  def shiftInterpolatedVol(diff : EnvironmentDifferentiable  with VolKey, dV : Quantity) : Environment = {
    val newInstrumentLevelEnvironment = ShiftInstrumentLevelVol(instrumentLevelEnv, diff, dV)
    val newEnv = copy(instrumentLevelEnv = newInstrumentLevelEnvironment)
    newEnv
  }

  def shiftIndexForwardPrice(index : SingleIndex, period : DateRange, dP : Quantity) = {
    copy(instrumentLevelEnv = ShiftSwapPrice(instrumentLevelEnv, index, period, dP))
  }

  def parallelShiftInterpolatedVols(curveKey : CurveKey, dV : Quantity) : Environment = {
    val newInstrumentLevelEnvironment = ParallelShiftInstrumentLevelVol(instrumentLevelEnv, curveKey, dV)

    copy(instrumentLevelEnv = newInstrumentLevelEnvironment)
  }

  def shiftMarketDayAtInstrumentLevel(newMarketDay : DayAndTime) : Environment = {
    copy(instrumentLevelEnv = ShiftMarketDayAtInstrumentLevel(instrumentLevelEnv, newMarketDay))
  }

  def calc_dP(market : CommodityMarket, period : DateRange): Quantity = {
    market.standardShift
  }

  override def toString = "Environment + " + instrumentLevelEnv.toString
  
}

object Environment{
  def apply(instrumentLevelEnv : AtomicEnvironment) : Environment = new Environment(new DefaultInstrumentLevelEnvironment(instrumentLevelEnv))
}

case class ObservationDay(day:Day) extends Ordered[ObservationDay] {
  def compare(other:ObservationDay) = day - other.day
}

