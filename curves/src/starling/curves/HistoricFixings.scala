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
    case observationDay: Day => key.index.fixing(marketDataSlice, observationDay)
  }
}

case class FixingsHistoryKey(index : SingleIndex)
  extends CurveKey {

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice): CurveObject = {
    FixingsHistory(marketDayAndTime, marketDataSlice, this)
  }

  def underlying = toString

  def typeName = "PriceFixingsHistory"

  def priceUOM = index.priceUOM
}


case class FixingKey(index : SingleIndex, observationDay: Day) extends AtomicDatumKey(FixingsHistoryKey(index), observationDay) {
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
  def nullValue = Quantity(1, key.market.priceUOM)
}
