package starling.curves

import starling.utils.cache.CacheFactory
import com.google.common.collect.MapMaker
import java.util.concurrent.FutureTask
import scala.collection.Iterator
import scala.collection.immutable.Set
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.quantity.{UOM, Percentage, Quantity}
import starling.marketdata._
import starling.daterange._
import starling.market.{CommodityMarket, SingleIndex, HasImpliedVol}

trait AtomicEnvironmentHelper{
  def apply(key : AtomicDatumKey) : Any

  def quantity(key : AtomicDatumKey) = apply(key).asInstanceOf[Quantity]
  def percentage(key : AtomicDatumKey) = apply(key).asInstanceOf[Percentage]

  def volMap(key : AtomicDatumKey) : DoubleMatrix2D = apply(key) match {
    case matrix : DoubleMatrix2D => matrix
    case untypedMap : Map[_,_] =>{
      val map = untypedMap.asInstanceOf[Map[Double, Percentage]]
      val xs = map.keySet.toArray
      val ys = xs.map{x => map(x).decimalValue}
      val m = new DenseDoubleMatrix2D(xs.size, 2)
      m.viewColumn(0).assign(xs)
      m.viewColumn(1).assign(ys)
      m
    }
  }
}
/**
 * For unit tests only as its properties are mutable
 *
 * @deprecated("Use UnitTestingAtomicEnvironment instead, it doesn't have mutable properties")
 */
abstract class TestingAtomicEnvironment(var shiftsCanBeIgnored : Boolean = false) extends AtomicEnvironment{
  def apply(key : AtomicDatumKey) : Any = {
    try {
      applyOrMatchError(key)
    } catch {
      case _ : MatchError => throw new MissingMarketDataException("No market data for " + key)
    }
  }
  def applyOrMatchError(key : AtomicDatumKey) : Any
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = {shiftsCanBeIgnored = canBeIgnored; this}
}

/**
 * <p>
 * 		An AtomicEnvironment contains those market data that can be viewed as atomic,
 * 		i.e. not depending on the value of any other data.
 * 		For example, forward fx rates are NOT atomic, as arbitrage dictates that these are
 * 		a function of a spot fx rate and two discount rates.
 * </p>
 * <p>
 * 		The following are examples of atomic market data
 * </p>
 * 	Prices, Spot FX, Discount rates
 * @see Environment
 */
trait AtomicEnvironment extends AtomicEnvironmentHelper{
  /**	Returns the value of a market datum
   * 	@param key Specifies an item of market data
   * 	@return The value of the market data
   */
  def apply(key : AtomicDatumKey) : Any
  def quantityInUSD(key: AtomicDatumKey, ccy: UOM) = quantity(key) * spotFXFor(ccy)
  def spotFXFor(ccy: UOM) = quantity(USDFXRateKey(ccy))

  /**
   * Atomic environments are joined in possibly long chains, each link decorating the behaviour through e.g.
   * shifting prices, moving the market day, making undiscounted.
   * Sometimes we want to change the behaviour of such a chain - for example making price perturbations
   * 'shallow' so that implied vol (which depends on price) can be left unaffected by price shifts
   * in order to calculate non-skew greeks.
   * This method will make shifts ignorable through the chain
   */
  def shiftsCanBeIgnored : Boolean
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) : AtomicEnvironment

  //  def quantity(key : AtomicDatumKey) = apply(key).asInstanceOf[Quantity]
  //  def percentage(key : AtomicDatumKey) = apply(key).asInstanceOf[Percentage]
  //
  //  def double(key : AtomicDatumKey) = apply(key).asInstanceOf[Double]
  //  def volMap(key : AtomicDatumKey) : DoubleMatrix2D = apply(key) match {
  //    case matrix : DoubleMatrix2D => matrix
  //    case untypedMap : Map[_,_] =>{
  //      val map = untypedMap.asInstanceOf[Map[Double, Percentage]]
  //      val xs = map.keySet.toArray
  //      val ys = xs.map{x => map(x).decimalValue}
  //      val m = new DenseDoubleMatrix2D(xs.size, 2)
  //      m.viewColumn(0).assign(xs)
  //      m.viewColumn(1).assign(ys)
  //      m
  //    }
  //  }
  /**	The market day and time of day (start/end) of this atomic environment
   */
  def marketDay : DayAndTime

}

case class UnitTestingAtomicEnvironment(marketDay : DayAndTime, data: PartialFunction[AtomicDatumKey, Any], shiftsCanBeIgnored : Boolean = false) extends AtomicEnvironment {

  override def apply(key: AtomicDatumKey) = {
    try {
      data(key)
    } catch {
      case _ : MatchError => throw new MissingMarketDataException("No market data for " + key)
    }
  }
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = copy(shiftsCanBeIgnored = canBeIgnored)
}

/**
 * Identifies a particular market datum as a point on a curve. For example forward prices
 * are specified by the curve to which they belong together with a day.
 *
 * the 'properties' allow us to modify how environments treat keys without subclassing, which was
 * has been a pain in the past. At present the only property is the ability to ignore shifts - this is
 * needed for non-skew greeks - i.e. want implied vol (which can be a function of price) to remain
 * unchanged in the presence of price shifts
 * 
 * @todo - rename as AtomicDatumIdentifier
 */
abstract class AtomicDatumKey(val curveKey : CurveKey, val point : AnyRef, val ignoreShiftsIfPermitted : Boolean = false){
  def forwardStateValue(originalAtomicEnv : AtomicEnvironment, forwardDayAndTime : DayAndTime) : Any

  def clearProperties : AtomicDatumKey = this
  def nullValue:Any

  /**
   * Same except for ignoreShiftsIfPermitted
   */
  def same(other: AtomicDatumKey) = other.getClass == this.getClass match {
    case true => curveKey == other.curveKey && point == other.point
    case false => false
  }

  def quantityValue(env : Environment) : Quantity = {
    env.instrumentLevelEnv.apply(this) match {
      case q : Quantity => q
      case p : Percentage => p : Quantity
      case x => throw new Exception("Don't support quantityValue for " + x)
    }
  }
}

/**
 * Indicates this is an AtomicDatumKey for vols
 */
trait VolKey

/**
 * Indicates this key can be bucketed
 */
trait Bucketable {
  self: AtomicDatumKey =>

  /**
   * Returns a key that gives the grouping for a bucket.
   * e.g. a forward price grouping would be
   * (ForwardPriceClass, Market)
   */
  def bucketGroup: Any

  /**
   * Creates a copy of this key with a new period. Used for bucketing
   */
  def bucket(period: Period): Bucketable
}

/** Implementations of this trait have access to CurveObjects. The values of atomic market data are
 * 	obtained by calling the relevant curve for each datum. Since implementations may build curves on
 * 	demand, a curve cache is maintained here.
 * 	<p>
 * 	Note that the term 'CurveObject' is used a little loosely. It strictly means some collection of
 * 	atomic market data. So there is no such thing in the system as a forward fx curve, although this
 * 	is common market terminology. Forward fx rates are determined by arbitrage in the Environment class.
 * 	@see Environment
 */
trait CurveObjectEnvironment extends AtomicEnvironment{
  /** Returns the curve object specified by the curve key
   */
  def curve(curveKey : CurveKey) : CurveObject

  /** A cache of curve objects
   */
  private var curveCache = CacheFactory.getCache("curveCache", unique = true)

  /** A cache of market data
   */
  private var dataCache = CacheFactory.getCache("dataCache", unique = true)

  /** Obtains market data by passing the request to the relevant CurveObject
   */
  def apply(key : AtomicDatumKey) : Any = {
    val curveKey = key.curveKey
    val value = dataCache.get[AtomicDatumKey, Any](key) match {
      case None => {
        val curveObject = curveCache.memoize(curveKey, curve(curveKey))
        dataCache.memoize(key, curveObject(key.point))
      }
      case Some(v) => v
    }
    value
  }

}

/** This implementation has no reference to curves, rather its market data is
 * maintained via a Map. Initially useful for unit tests only, although this
 * could be used later for serialization.
 */
case class MappingAtomicEnvironment(
  map : Map[AtomicDatumKey, Quantity],
  marketDay : DayAndTime,
  shiftsCanBeIgnored : Boolean = false
)
	extends AtomicEnvironment
{
  def apply(key : AtomicDatumKey) : Quantity = {
    map.get(key) match {
      case Some(value) => value
      case None => throw new MissingMarketDataException("No data for " + key)
    }
  }

  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = copy(shiftsCanBeIgnored = canBeIgnored)
}

/** Currently only used in unit tests, this implementation maintains
 * its own set of curves in a map.
 *
 */
case class MappingCurveObjectEnvironment(
  curves : Map[CurveKey, CurveObject],
  marketDay : DayAndTime,
  shiftsCanBeIgnored : Boolean = false
)
	extends CurveObjectEnvironment
{
  def curve(curveKey : CurveKey) : CurveObject = curves.get(curveKey) match {
    case Some(curve) => curve
    case None => throw new MissingMarketDataException("No curve for " + curveKey)
  }

  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = copy(shiftsCanBeIgnored = canBeIgnored)
}


/**
 * An implementation of CurveObjectEnvironment which uses a ExternalMarketDataReader
 */
case class MarketDataCurveObjectEnvironment(
  marketDayAndTime:DayAndTime,
  marketDataSlice:MarketDataSlice,
  shiftsCanBeIgnored : Boolean = false
) extends CurveObjectEnvironment {

  def marketDay = marketDayAndTime
  def curve(curveKey : CurveKey) : CurveObject = read(curveKey)

  def read(key: CurveKey): CurveObject = {
    key.buildFromMarketData(marketDayAndTime, marketDataSlice)
  }

  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = copy(shiftsCanBeIgnored = canBeIgnored)
}

/**
 * Contains all market data needed for a standard Environment
 */
trait MarketDataSlice {
  def read(key:MarketDataKey): MarketData
  // throw new MissingMarketDataException("No " + index + " fixing for " + day)
  def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint): PriceFixingsHistoryData
}
