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

trait ForwardCurveLookup {
  def priceFromForwardPrices(prices: Map[DateRange, Quantity], dateRange: DateRange): Quantity
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
  require(prices.values.forall(_.uom == market.priceUOM), "Not the same UOM, market: " + market.priceUOM + ", prices: " + prices)

  private lazy val memoizedPrices = CacheFactory.getCache("ForwardCurve", unique = true)

  def price(dateRange: DateRange) = {
    assert(!prices.isEmpty, "No available prices for market " + market + " on " + marketDayAndTime)
    val p: Quantity = try {
      market.priceFromForwardPrices(prices, dateRange)
    } catch {
      case e => throw new MissingPriceException(market.toString, dateRange, marketDayAndTime + ": Failed to get price for " + dateRange + " on market " + market + ". Prices " + prices, e)
    }
    memoizedPrices.memoize(dateRange, p)
  }

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
