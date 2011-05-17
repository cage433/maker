package starling.db

import starling.daterange.{ObservationTimeOfDay, Day, ObservationPoint}
import starling.marketdata._

class ValidatingMarketDataReader(reader: MarketDataReader, validators: PriceValidator*) extends MarketDataReaderAdapter(reader) {
  val validator = new CompositePriceValidator(validators)

  override def read(marketDataType: MarketDataType, observationDays: Option[Set[Option[Day]]],
                    observationTimes: Option[Set[ObservationTimeOfDay]], keys: Option[Set[MarketDataKey]]) = {
    super.read(marketDataType, observationDays, observationTimes, keys).map {
      case (point, key, data) => (point, key, validate(key, point, data))
    }
  }

  override def readAllPrices(observationPoint: ObservationPoint) = super.readAllPrices(observationPoint).map {
    case (key, data) => (key, validate(key, observationPoint, data))
  }

  override def readAllVols(observationPoint: ObservationPoint) = super.readAllVols(observationPoint)

  private def validate[T](key: MarketDataKey, observationPoint: ObservationPoint, data: T): T = data match {
    case priceData: PriceData => validator.validate(key, observationPoint, priceData).asInstanceOf[T]
    case other => other
  }
}