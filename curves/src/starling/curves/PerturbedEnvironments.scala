package starling.curves

import starling.varcalculator.ForwardRateRiskFactor
import starling.marketdata.MarketData
import starling.quantity.{Percentage, UOM, Quantity}
import starling.quantity.UOM._
import starling.utils.cache.CacheFactory
import com.google.common.collect.MapMaker
import java.lang.String
import scala.collection.immutable.Set
import starling.utils.Log
import java.util.concurrent.{ConcurrentHashMap, FutureTask}
import starling.market._
import starling.daterange._

abstract class DerivedAtomicEnvironment(
  originalEnv : AtomicEnvironment
)
  extends AtomicEnvironment{

  def marketDay = originalEnv.marketDay
  def shiftsCanBeIgnored = originalEnv.shiftsCanBeIgnored

  def setShiftsCanBeIgnored(canBeIgnored : Boolean) : AtomicEnvironment = {
    makeCopyWithNewChildEnvironment(originalEnv.setShiftsCanBeIgnored(canBeIgnored))
  }
  def makeCopyWithNewChildEnvironment(newEnv : AtomicEnvironment) : AtomicEnvironment
}

abstract class PerturbedAtomicEnvironment(
  originalEnv : AtomicEnvironment
)
  extends DerivedAtomicEnvironment(originalEnv){

  def apply(key: AtomicDatumKey) : Any= {
    if (key.ignoreShiftsIfPermitted && shiftsCanBeIgnored)
      originalEnv(key)
    else
      applyWithShiftIfAppropriate(key)
  }

  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any
}

case class GenericPerturbedAtomicEnvironment(name: String, originalEnv : AtomicEnvironment, pertubation: PartialFunction[AtomicDatumKey, Any])
  extends PerturbedAtomicEnvironment(originalEnv) {

  def applyWithShiftIfAppropriate(key: AtomicDatumKey) : Any = {
    try {
      pertubation(key)
    } catch {
      case _ : MatchError => originalEnv.apply(key)
    }
  }

  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }

  override def toString = name
}

/**
 * Contains various classes which perturb environments. Used in VaR scenario generation,
 * risk calculation and P&L Explanation
 */

case class OverrideForCurveKeysEnvironment(
        originalEnv : AtomicEnvironment,
        curveKeys:Set[CurveKey],
        alternativeEnv:AtomicEnvironment) extends PerturbedAtomicEnvironment(originalEnv) {

  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    if (curveKeys.contains(key.curveKey)){
      alternativeEnv(key)
    } else {
      originalEnv(key)
    }
  }

  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }
}
/** An AtomicEnvironment that overrides a single curve. The values for all
 * 	other curves come from the original environment.
 */
case class OverrideCurveObjectEnvironment (
  originalEnv : AtomicEnvironment,
  perturbedCurves : Map[CurveKey, CachingCurveObject]
)
	extends PerturbedAtomicEnvironment(originalEnv)
{


  /** Obtains the market data from the either the overridden curve object
   * or the original environment as appropriate
   */
  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    if (perturbedCurves.contains(key.curveKey)){
      perturbedCurves(key.curveKey)(key.point)
    } else {
      originalEnv(key)
    }
  }
  /** All instances of this object will be created through this constructor which
   * guarantees that the curve objects cache their results.
   */
  def this(originalEnv : AtomicEnvironment, perturbedCurveKey : CurveKey, perturbedCurveObject : CurveObject) =
    this(originalEnv,
      Map((perturbedCurveKey, perturbedCurveObject.asCachingObject)) // scala bug #1974
    )
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }
}

/**
 * An environment, used for VaR, that will fall back to another environment
 * if a MissingMarketDataException is thrown. The other environment is usually
 * from a previous day.
 * Specify keyMatch so that it only uses the fallbackEnv for certain keys.
 */
case class FallbackCurveObjectEnvironment(
        originalEnv: AtomicEnvironment,
        fallbackEnv: AtomicEnvironment,
        keyMatch: PartialFunction[AtomicDatumKey, Boolean] = {case k => true}
        )
        extends PerturbedAtomicEnvironment(originalEnv)
{

  /**Obtains the market data from the either the overridden curve object
   * or the original environment as appropriate
   */
  def applyWithShiftIfAppropriate(key: AtomicDatumKey): Any = {
    try {
      originalEnv(key)
    } catch {
      case m: MissingMarketDataException => {
        if(keyMatch(key)) {
          fallbackEnv(key)
        } else {
          throw m
        }
      }
    }
  }

  def makeCopyWithNewChildEnvironment(newChildEnv: AtomicEnvironment) = {
    copy(originalEnv = newChildEnv)
  }
}

/**
 * An AtomicEnvironment that perturbs a atomic data. Using a single
 * perturbation permits calculation of numeric deltas with respect to that datum.
 */
case class AtomicEnvironmentWithPerturbationMap(
  originalEnv : AtomicEnvironment,
  shifts : Map[AtomicDatumKey, Quantity]
)
  extends PerturbedAtomicEnvironment(originalEnv)
{
	def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    if (shifts.contains(key))
      originalEnv.quantity(key) + shifts(key)
    else
      originalEnv(key)
	}
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }
}

case class ShiftEquityPrice(env : AtomicEnvironment, ric : RIC, dP : Quantity)
  extends PerturbedAtomicEnvironment(env)
{
  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    key match {
      case EquityPriceKey(`ric`) => {
        val price = env.quantity(key)
        if (price.uom != dP.uom) {
          throw new Exception("Price units differ for " + ric + " price: " + price + " shift: " + dP)
        }
        price + dP
      }
      case _ => env(key)
    }
  }
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(env = newChildEnv)
  }
}

case class ShiftSpotFX(env : AtomicEnvironment, ccy : UOM, dP : Quantity)
  extends PerturbedAtomicEnvironment(env)
{
  require(dP.uom == USD / ccy)
  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    key match {
      case USDFXRateKey(`ccy`) => env.quantity(key) + dP
      case _ => env(key)
    }
  }
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(env = newChildEnv)
  }
}

case class ShiftForwardForwardRate(env : AtomicEnvironment, ccy : UOM, period : DateRange, dR : Quantity)
  extends PerturbedAtomicEnvironment(env)
{
  assert(dR.uom == UOM.SCALAR, "Can only shift IR curves with scala units")

  def applyWithShiftIfAppropriate(key: AtomicDatumKey) : Any = {
    key match {
      case DiscountRateKey(ccy, day, _) => env.quantity(key) * ForwardRateRiskFactor.adjustment(env.marketDay.day, period, day, dR)
      case _ => env(key)
    }
  }
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(env = newChildEnv)
  }
}
/**
 * Modifies an atomic environment to be undiscounted
 */
case class Undiscounted(env : AtomicEnvironment)
  extends PerturbedAtomicEnvironment(env)
{
  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    key match {
      case _ : DiscountRateKey => new Quantity(1.0)
      case _ => env(key)
    }
  }
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(env = newChildEnv)
  }
}

/**
 * Modifies an atomic environment to have zero vols
 */
case class ZeroVols(env : AtomicEnvironment)
  extends PerturbedAtomicEnvironment(env)
{
  def applyWithShiftIfAppropriate(key : AtomicDatumKey) : Any = {
    key match {
      case _ : BradyFXVolSmileAtomicDatumKey => Map(0.5 -> Percentage(0))
      case _ : OilAtmVolAtomicDatumKey => Percentage(0)
      case _ : OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
      case _ : BradyMetalVolAtomicDatumKey => Percentage(0)
      case k : SpreadAtmStdDevAtomicDatumKey => new Quantity(0.0, k.market.priceUOM)
      case _ : SpreadSkewStdDevAtomicDatumKey => Map(0.2 -> Percentage(0), 0.5 -> Percentage(0), 0.8 -> Percentage(0))
      case _ => env(key)
    }
  }
  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(env = newChildEnv)
  }
}

/** Adds a cache to an AtomicEnvironment. All values are obtained
 * 	from the original environment and cached here.
 */
case class CachingAtomicEnvironment(
  originalEnv : AtomicEnvironment
)
	extends DerivedAtomicEnvironment(originalEnv)
{
  private val memoized = CacheFactory.getCache("CachingAtomicEnvironment", unique = true)

	def apply(key : AtomicDatumKey) : Any = memoized.memoize(key, originalEnv(key))

  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }

}

case class NullAtomicEnvironment(marketDay:DayAndTime, shiftsCanBeIgnored : Boolean = false) extends AtomicEnvironment {
  def apply(key : AtomicDatumKey) : Any = key.nullValue
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = copy(shiftsCanBeIgnored = canBeIgnored)
}

/**
 * Records all keys used to get values from originalEnv. Values are not stored.
 * Note: Values returned are not necessarily market values. They can be mock values to avoid
 * throwing an exception and missing keys.
 */
case class KeyRecordingCurveObjectEnvironment(
   originalEnv : AtomicEnvironment
)
  extends DerivedAtomicEnvironment(originalEnv)
{
  import scala.collection.JavaConversions._
  import  starling.utils.CollectionUtils._
  private val record = new MapMaker().concurrencyLevel(16).makeMap[AtomicDatumKey, FutureTask[AtomicDatumKey]]

  def keys: Set[AtomicDatumKey] = {
    Set[AtomicDatumKey]() ++ record.keySet
  }


  override def apply(key: AtomicDatumKey): Any = {
    putIfAbsent(record, key, key)
    try {
      originalEnv(key)
    }
    catch {
      case _ => key.nullValue
    }
  }

  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }
}

/**
 * Records all keys used and all values returned from originalEnv
 */
case class KeyAndValueRecordingCurveObjectEnvironment(
  originalEnv : AtomicEnvironment
)
  extends DerivedAtomicEnvironment(originalEnv)
{
  import scala.collection.JavaConversions._
  import  starling.utils.CollectionUtils._
  private val record = new ConcurrentHashMap[AtomicDatumKey, FutureTask[Any]]()

  def keysAndValues:Map[AtomicDatumKey, Any] = Map() ++ (for((k, v) <- record) yield (k -> v.get))

  def clear = record.clear

  override def apply(key: AtomicDatumKey): Any = {
    val value = originalEnv(key)
    putIfAbsent(record, key, value)
    value
  }

  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }
}

case class ForwardStateEnvironment(
  originalEnv : AtomicEnvironment,
  val marketDay : DayAndTime
)
  extends AtomicEnvironment
{
  require(marketDay >= originalEnv.marketDay, "Can't move environment backwards in time (" + originalEnv.marketDay + " to " + marketDay + ")")

  def apply(key: AtomicDatumKey) = key.forwardStateValue(originalEnv, marketDay)
  def shiftsCanBeIgnored = originalEnv.shiftsCanBeIgnored

  def setShiftsCanBeIgnored(canBeIgnored : Boolean) : AtomicEnvironment = copy(originalEnv = originalEnv.setShiftsCanBeIgnored(canBeIgnored))
}

/**
 * Used to explain P&L changes due to time changing. Moves the discounting forward by one
 * day, without changing the market day. Note that the discount rate for today and the
 * forward day will both be 1.0. This is consistent with the view that P&L is booked to an
 * account that bears no interest. Not tru - but consistent with Trader's P&L
 */
case class ForwardDiscountingEnvironment(
  originalEnv: AtomicEnvironment,
  val forwardDay: Day
)
  extends PerturbedAtomicEnvironment(originalEnv)
{
  require(marketDay >= originalEnv.marketDay, "Can't move environment backwards in time")

  def applyWithShiftIfAppropriate(key: AtomicDatumKey) =
    key match {
      case DiscountRateKey(_, day, _) => {
        if (day <= forwardDay)
          new Quantity(1.0)
        else
          key.forwardStateValue(originalEnv, forwardDay.startOfDay)
      }
      case _ => originalEnv(key)
    }

  def makeCopyWithNewChildEnvironment(newChildEnv : AtomicEnvironment) ={
   copy(originalEnv = newChildEnv)
  }
}



