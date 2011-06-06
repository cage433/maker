package starling.curves

import starling.market._
import starling.quantity.Quantity
import starling.daterange.ObservationPoint
import starling.daterange._


/** A history of fixings for a given market -
 *  This currently conflates two things - spot fixings and front futures prices.
 */
case class FixingsHistory(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice, key: FixingsHistoryKey)
  extends CurveObject {

  type CurveValuesType = Quantity

  def apply(point : AnyRef) : Quantity = point match {
    case day: Day => marketDataSlice.fixings(key.market, ObservationPoint(day, key.observationTime))
                                    .fixingFor(key.level, key.period.storedPeriod)
                                    .toQuantity
  }
}

case class FixingsHistoryKey(market:CommodityMarket, period: FixingPeriod, level:Level, observationTime:ObservationTimeOfDay)
  extends CurveKey {

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice): CurveObject = {
    FixingsHistory(marketDayAndTime, marketDataSlice, this)
  }

  def underlying = toString

  def typeName = "PriceFixingsHistory"

  def priceUOM = market.priceUOM
}


case class FixingKey(key: FixingsHistoryKey, day: Day) extends AtomicDatumKey(key, day) {
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    if (day.endOfDay <= originalAtomicEnv.marketDay) {
      originalAtomicEnv(this)
    } else if (day.endOfDay <= forwardDayAndTime) {
      val env = Environment(originalAtomicEnv)
      env.forwardPrice(key.market, key.period.period(day))
    } else {
      throw new Exception("Can't get fixing for day in the future")
    }
    
  }
  def nullValue = Quantity(1, key.market.priceUOM)
}
