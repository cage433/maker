package starling.db

import starling.curves.MissingMarketDataException
import starling.daterange.{ObservationTimeOfDay, Day, ObservationPoint}
import starling.marketdata._

import starling.utils.ImplicitConversions._


trait MarketDataReader {
  //Abstract
  def identifier:String

  def marketDataTypes:List[MarketDataType]

  def read(marketDataType: MarketDataType, observationDays: Option[Set[Option[Day]]] = None,
           observationTimes: Option[Set[ObservationTimeOfDay]] = None,
           keys: Option[Set[MarketDataKey]]=None): List[(TimedMarketDataKey, MarketData)]

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataType): List[TimedMarketDataKey]

  def readAll(marketDataType: MarketDataType, observationPoint:ObservationPoint) = read(
    marketDataType,
    observationDays = Some(Set(observationPoint.day)),
    observationTimes = Some(Set(observationPoint.timeOfDay))
  ).mapFirst(_.key)

  //Helper concrete methods
  def read(timedKey: TimedMarketDataKey): MarketData = {
    val key = timedKey.key
    val observationPoint = timedKey.observationPoint
    read(key.dataType, Some(Set(observationPoint.day)), Some(Set(observationPoint.timeOfDay)), Some(Set(key))).headOption match {
      case Some((_,marketData)) => marketData
      case None => throw new MissingMarketDataException(
        "No data found for " + identifier + " " + key + " for " + observationPoint)
    }
  }

  def readAs[T <: MarketData](timedKey: TimedMarketDataKey) = read(timedKey).asInstanceOf[T]

  def readAllPrices(observationPoint:ObservationPoint):List[(PriceDataKey, PriceData)] = {
    read(PriceDataType, Some(Set(observationPoint.day)), Some(Set(observationPoint.timeOfDay)))
      .mapFirst(_.key).filterCast[(PriceDataKey, PriceData)]
  }

  def readAllVols(observationPoint: ObservationPoint):List[(OilVolSurfaceDataKey, OilVolSurfaceData)] = {
    read(OilVolSurfaceDataType, Some(Set(observationPoint.day)), Some(Set(observationPoint.timeOfDay)))
      .mapFirst(_.key).filterCast[(OilVolSurfaceDataKey, OilVolSurfaceData)]
  }
}
