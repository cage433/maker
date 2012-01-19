package starling.curves

import starling.daterange.DayAndTime
import starling.utils.cache.CacheFactory
import starling.market.HasImpliedVol
import starling.marketdata.{ReferenceDataLookup, MarketDataTypeName, MarketDataKey, MarketData}


/**
 * Each AtomicMarketDatum has an associated curve key. these keys
 * know how to build curves from the database. They are also used to cache
 * curve objects.
 * @todo - rename to CurveIdentifier
 * @see AtomicMarketDatum
 */
trait CurveKey {
  def buildFromMarketData(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice, refData : ReferenceDataLookup): CurveObject

  /**
   * A string that represents the underlying for this key. E.g. a price key for ICE WTI would
   * have an underlying of "ICE WTI"
   */
  def underlying: String

  /**
   * A string that represents the parent underlying for this key. E.g. a price key for ICE WTI would
   * have an underlying of "WTI" (Commodity basically)
   */
  def higherUnderlying = ""

  /**
   * The type of key, e.g. "Price", "Vol", "Vol Skew" .. etc.
   */
  def typeName: MarketDataTypeName
}

trait NonHistoricalCurveKey[T <: MarketData] extends CurveKey {
  def buildFromMarketData(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice, refData : ReferenceDataLookup): CurveObject = {
    buildFromMarketData(marketDayAndTime, marketDataSlice.read(marketDataKey).asInstanceOf[T], refData)
  }

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: T, refData : ReferenceDataLookup): CurveObject

  def marketDataKey: MarketDataKey

  def typeName = marketDataKey.typeName
}

trait VolCurveKey {
  self: CurveKey =>

  def volMarket: HasImpliedVol

  def underlying: String = volMarket.name
}

/**
 * This will be the super class of ForwardCurve, DiscountCurve, VolSurface etc.
 * <p>
 * Curve objects can be though of as a function of 'points', where the type of
 * a point will depend on the implementation. For discount curves and daily forward 
 * curves a point would be a day. Futures markets that only trade monthly markets
 * would normally use months as curve points. Vol surfaces might use a 
 * delivery period/strike pair.
 */
trait CurveObject{

  type CurveValuesType
  /** Each CurveObject implementation knows how to retrieve values for 
   * 	'points' in its curve.
   */
  def apply(point : AnyRef) : CurveValuesType
  
  /** Converts this CurveObject into an equivalent one which caches its 
   *  values. This is to avoid, for example, repeated calls to interpolation 
   * 	functions.
   */
  def asCachingObject : CachingCurveObject = {
    if (this.isInstanceOf[CachingCurveObject])
      this.asInstanceOf[CachingCurveObject]
    else
    	CachingCurveObject(this)
  }

  val marketDayAndTime : DayAndTime
  def marketDay = marketDayAndTime.day
}

/** Adds a cache to a curve object to avoid repeated calls to relatively expensive
 * interpolation functions etc.
 */
case class CachingCurveObject (originalObject : CurveObject)
	extends CurveObject
{
  type CurveValuesType = originalObject.CurveValuesType
  import scala.collection.mutable
  
	private val cache = CacheFactory.getCache("CachingCurveObject", unique = true)

  def apply(point : AnyRef) : CurveValuesType = cache.memoize(point, originalObject(point))

  val marketDayAndTime = originalObject.marketDayAndTime
}

