package starling.curves

import starling.daterange._
import starling.market._
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.utils.Log
import starling.utils.cache.CacheFactory
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.pivot.PivotQuantity
import starling.marketdata.{FreightFlatRateData, FreightFlatRateDataKey, PriceData, PriceDataKey}

case class FreightFlatRateCurveKey(market: CommodityMarket) extends NonHistoricalCurveKey[FreightFlatRateData] {

  def marketDataKey = FreightFlatRateDataKey(market.marketDataMarket)

  def buildFromMarketData(marketDayAndTime: DayAndTime, data: FreightFlatRateData) = {
    if (data.isEmpty) {
      throw new MissingMarketDataException("No market data for " + market + " on " + marketDayAndTime)
    } else {
      new FreightFlatRateCurve(market, marketDayAndTime, data.prices.mapValues(_.quantityValue.get))
    }
  }

  def underlying = market.name

  override def higherUnderlying = market.commodity.name
}

case class FreightFlatRateCurve(
                                 market: CommodityMarket,
                                 marketDayAndTime: DayAndTime,
                                 prices: Map[Year, Quantity]
                                 )
  extends CurveObject with ForwardCurveTrait {
  def price(dateRange: DateRange) = dateRange match {
    case y: Year => prices(y)
    case _ => throw new IllegalArgumentException(dateRange + " is not a valid daterange")
  }

  type CurveValuesType = Quantity
}

case class FreightFlatRateKey(
                            market: CommodityMarket,
                            year: Year,
                            override val ignoreShiftsIfPermitted: Boolean = false
                            )
  extends AtomicDatumKey(FreightFlatRateCurveKey(market), year, ignoreShiftsIfPermitted) {

  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv(this)
  }

  override def clearProperties: AtomicDatumKey = copy(ignoreShiftsIfPermitted = false)

  def bucket(period: Period) = period match {
    case DateRangePeriod(y: Year) => copy(year = y)
  }

  def bucketGroup = (this.getClass, market)

  def nullValue = Quantity(10, FreightFlatRateForwardCurve.priceUOM)
}

object FreightFlatRateForwardCurve {
  val priceUOM = USD/MT // Worldscale is actually USD/MT

  def create(market: CommodityMarket, marketDayAndTime: DayAndTime, prices: Map[Year, Double]) = {
    new FreightFlatRateCurve(market, marketDayAndTime, prices.mapValues(Quantity(_, priceUOM)))
  }
}

