package starling.db

import starling.curves.{MissingMarketDataException, ObservationDay}
import starling.daterange.{ObservationTimeOfDay, Day, ObservationPoint}
import starling.marketdata._


trait MarketDataReader {
  //Abstract
  def identifier:String

  def marketDataTypes:List[MarketDataType]

  def read(marketDataType: MarketDataType, observationDays: Option[Set[Option[Day]]] = None,
           observationTimes: Option[Set[ObservationTimeOfDay]] = None,
           keys: Option[Set[MarketDataKey]]=None): List[(ObservationPoint, MarketDataKey, MarketData)]

  def readAllObservationDayAndMarketDataKeys(marketDataType: MarketDataType): List[(ObservationPoint, MarketDataKey)]

  def readAll(marketDataType: MarketDataType, observationPoint:ObservationPoint) = {
    read(
      marketDataType,
      observationDays = Some(Set(observationPoint.day)),
      observationTimes = Some(Set(observationPoint.timeOfDay))
    ).map {
      case (_, key, data) => (key,data)
    }
  }

  //Helper concrete methods
  def read(observationPoint:ObservationPoint, key:MarketDataKey): MarketData = {
    read(key.dataType, Some(Set(observationPoint.day)), Some(Set(observationPoint.timeOfDay)), Some(Set(key))).headOption match {
      case Some((_,_,marketData)) => marketData
      case None => throw new MissingMarketDataException(
        "No data found for " + identifier + " " + key + " for " + observationPoint)
    }
  }

  def readAs[T <: MarketData](observationPoint:ObservationPoint, key:MarketDataKey) = read(observationPoint, key).asInstanceOf[T]

  def readAllPrices(observationPoint:ObservationPoint):List[(PriceDataKey, PriceData)] = {
    read(PriceDataType, Some(Set(observationPoint.day)), Some(Set(observationPoint.timeOfDay))).map {
      case (point,key:PriceDataKey,data:PriceData) => (key,data)
    }
  }

  def readAllVols(observationPoint: ObservationPoint):List[(OilVolSurfaceDataKey, OilVolSurfaceData)] = {
    read(OilVolSurfaceDataType, Some(Set(observationPoint.day)), Some(Set(observationPoint.timeOfDay))).map {
      case (point,key:OilVolSurfaceDataKey,data:OilVolSurfaceData) => (key,data)
    }
  }
}
