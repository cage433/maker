package starling.curves

import starling.daterange._
import starling.market._
import starling.marketdata.{PriceData, PriceDataKey}
import starling.quantity.Quantity
import starling.utils.Log
import starling.utils.cache.CacheFactory
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.pivot.PivotQuantity


trait HasInterpolation {
  def interpolation: InterpolationMethod
}






case class ForwardCurveKey(market : CommodityMarket) extends NonHistoricalCurveKey[PriceData] {

  def marketDataKey = {
    market match {
      case brent : IsBrentMonth => brent.marketDataKey
      case _ => PriceDataKey(market.marketDataMarket)
    }
  }

  def buildFromMarketData(marketDayAndTime: DayAndTime, priceData: PriceData) = {
    if (priceData.isEmpty) {
      throw new MissingMarketDataException("No market data for " + market + " on " + marketDayAndTime)
    } else {
      def createForwardCurve(prices:Map[DateRange,PivotQuantity]) = {
        ForwardCurve(market, marketDayAndTime, Map() ++ (for((dateRange, price) <- prices if (dateRange.lastDay >= marketDayAndTime.day)) yield {
          if (price.doubleValue.get < 0) {
            Log.warn("Negative price found for " + market + " with " + dateRange +  " on " + marketDayAndTime + ". Replacing with its absolute value")
            dateRange → (-price.quantityValue.get)
          } else {
            dateRange → price.quantityValue.get
          }
        }))
      }
      market match {
        case brent : IsBrentMonth => {
          val monthNumber = brent.month.month
          createForwardCurve(
            priceData.prices.find { case (dr, p) => dr.firstDay.month == monthNumber }.map{case (dr, p) => dr.firstDay → p}.toMap
          )
        }
        case _ => createForwardCurve(priceData.prices)
      }
    }
  }

  def underlying = market.name

  override def higherUnderlying = market.commodity.name
}

trait PriceKey{
  def market : CommodityMarket
}

case class ForwardPriceKey(
  market : CommodityMarket,
  period : DateRange,
  override val ignoreShiftsIfPermitted : Boolean = false
)
	extends AtomicDatumKey(ForwardCurveKey(market), period, ignoreShiftsIfPermitted) 
{
  market.tenor match {
    case Day if market.commodity != Freight => assert(period.isInstanceOf[Day], "Market " + market + " doesn't support prices for non daily periods, received " + period)
    case Month => assert(period.isInstanceOf[Month], "Market " + market + " doesn't support prices for non monthly periods, received " + period)
    case _ =>
  }
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    market match {
      case f: FuturesMarket => {
        if(period < f.frontPeriod(forwardDayAndTime.day)) {
          println("???!?")
        }
        assert(
        period >= f.frontPeriod(forwardDayAndTime.day),
        "Future " + this + " has already expired on " + forwardDayAndTime
        )
      }
      case _ =>
    }
    originalAtomicEnv(this)
  }

  override def clearProperties : AtomicDatumKey = copy(ignoreShiftsIfPermitted = false)

  def bucket(period: Period) = period match {
    case DateRangePeriod(p) => copy(period = p)
  }

  def bucketGroup = (this.getClass, market)

  def nullValue = Quantity(10, market.priceUOM)
}

trait ForwardCurveTrait{
  def apply(point : AnyRef) = {
    point match {
      case dateRange : DateRange => price(dateRange)
    }
  }
  def price(dateRange : DateRange) : Quantity
}

case class ConstantForwardCurve(
  marketDayAndTime : DayAndTime,
  market : CommodityMarket,
  price : Quantity
)
	extends CurveObject with ForwardCurveTrait
{
  type CurveValuesType = Quantity
  def price(dateRange: DateRange) = price
}
object ForwardCurve {
  def create(market : CommodityMarket, marketDayAndTime : DayAndTime, prices : Map[DateRange, Double]) = {
    new ForwardCurve(market, marketDayAndTime, prices.mapValues(Quantity(_, market.priceUOM)))
  }
}
/** A simple forward curve. Interpolation between the supplied day prices is handled by the market.
 * <p>
 */
case class ForwardCurve(
  market : CommodityMarket,
  marketDayAndTime : DayAndTime,
  prices : Map[DateRange, Quantity]
)
	extends CurveObject with ForwardCurveTrait
{
  type CurveValuesType = Quantity
  require(prices.keySet.forall(_.lastDay >= marketDayAndTime.day), "Forward curve for " + market + ", " + marketDayAndTime + " has prices in the past")

  private lazy val memoizedPrices = CacheFactory.getCache("ForwardCurve", unique = true)

  private lazy val orderedDays = prices.asInstanceOf[Map[Day, Double]].keySet.toList.sortWith(_ < _).toArray
  private lazy val orderedPrices = orderedDays.map(prices(_).value).toArray

  def price(dateRange: DateRange) = {
    assert(!prices.isEmpty, "No available prices for market " + market + " on " + marketDayAndTime)
    def p = {
      market match {
        case m: HasInterpolation => dateRange match {
          case day: Day => Quantity(interpolate(m, day), market.priceUOM)
          case _ => throw new Exception("Market " + market + " is daily so does not directly support prices for " + dateRange)

        }
        case _ => prices.get(dateRange) match {
          case Some(p) => p
          case None => {
            // OK this sucks. There occasionally appear to be missing monthly prices in Trinity. Unfortunately at this
            // point we don't know if the data came from Trinity or FC. So... if the commodity is a metal we are adding
            // a hack to simply use a proxy price
            market.commodity match {
              case _ : MetalCommodity => {
                val periods = prices.keySet.toList.sortWith(_.firstDay < _.firstDay)
                periods.find(_.firstDay >= dateRange.firstDay) match {
                  case Some(period) => prices(period)
                  case _ => prices(periods.last)
                }
              }
              case _ =>throw new MissingPriceException(market.toString, dateRange, marketDayAndTime+": No price for " + dateRange + " on market " + market + ". Prices " + prices)

            }
          }
        }
      }
    }
    memoizedPrices.memoize(dateRange, p)
  }

  def interpolate(market: HasInterpolation, day: Day) = market.interpolation.interpolate(orderedDays, orderedPrices, day)

  def applyShifts(shifts : Array[Double]) : ForwardCurve = {
    assert(shifts.size == prices.size, "Shifts and prices are not the same size")
    val orderedDelivery = prices.keySet.toList.sortWith(_ < _)
    val newPrices = Map.empty[DateRange, Quantity] ++ orderedDelivery.toList.zip(shifts.toList).map{
      case (dateRange, dP) => {
        val price: Quantity = prices(dateRange)
        (dateRange -> (price + Quantity(dP, price.uom)))
      }
    }
    ForwardCurve(market, marketDayAndTime, newPrices)
  }
}
