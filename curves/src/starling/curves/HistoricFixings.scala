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
    case (observationDay: Day, period: Option[StoredFixingPeriod]) => {
      key.fixing.fixing(marketDataSlice, observationDay, period)
    }
  }
}

trait FixingHistoryLookup {
  def fixing(mds: MarketDataSlice, observationDay: Day, storedFixingPeriod: Option[StoredFixingPeriod]): Quantity
}

case class FixingsHistoryKey(fixing: FixingHistoryLookup)
  extends CurveKey {

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice): CurveObject = {
    FixingsHistory(marketDayAndTime, marketDataSlice, this)
  }

  def underlying = toString

  def typeName = "PriceFixingsHistory"
}


case class IndexFixingKey(index : SingleIndex, observationDay: Day)
  extends AtomicDatumKey(FixingsHistoryKey(index), (observationDay, None)) {

  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    if (observationDay.endOfDay <= originalAtomicEnv.marketDay) {
      originalAtomicEnv(this)
    } else if (observationDay.endOfDay <= forwardDayAndTime) {
      val env = Environment(originalAtomicEnv)
      env.fixingOrForwardPrice(index, observationDay)
    } else {
      throw new Exception("Can't get fixing for day in the future")
    }
    
  }
  def nullValue = Quantity(1, index.priceUOM)
}

case class MarketFixingKey(market: CommodityMarket, observationDay: Day, forwardDate: DateRange)
  extends AtomicDatumKey(FixingsHistoryKey(market), (observationDay, Some(StoredFixingPeriod.dateRange(forwardDate)))) {

  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    if (observationDay.endOfDay <= originalAtomicEnv.marketDay) {
      originalAtomicEnv(this)
    } else if (observationDay.endOfDay <= forwardDayAndTime) {
      val env = Environment(originalAtomicEnv)
      env.forwardPrice(market, forwardDate)
    } else {
      throw new Exception("Can't get fixing for day in the future")
    }
  }

  def nullValue = Quantity(1, market.priceUOM)
}
