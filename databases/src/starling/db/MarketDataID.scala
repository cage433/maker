package starling.db

import starling.daterange.ObservationPoint
import starling.quantity.{Percentage, Quantity}
import starling.pivot.{Field, PivotQuantity}
import starling.instrument.utils.StarlingXStream
import starling.utils.Log
import starling.utils.ImplicitConversions._
import collection.Iterable
import starling.marketdata.{MarketDataValueKey, MarketData, TimedMarketDataKey, MarketDataKey}

/**
 * Uniquely identifies a market data object in the database. Versioning is added
 * on top of this by the VersionedDatabase mixin.
 */
case class MarketDataID(observationPoint: ObservationPoint, marketDataSet: MarketDataSet, subTypeKey: MarketDataKey) extends Log {
  def conditions : Map[String, Any] = Map(
    "observationTime" -> observationPoint.timeName,
    "observationDay" -> observationPoint.day.getOrElse(null),
    "marketDataSet" -> marketDataSet.name,
    "marketDataType" -> StarlingXStream.write(subTypeKey.dataType),
    "marketDataKey" -> StarlingXStream.write(subTypeKey))

  def extractValue(row: Map[Field, Any]): Option[(String, Double)] = subTypeKey.dataType.getValues(row) match {
    case List(Quantity(value, uom)) => Some((uom.toString, value))
    case List(PivotQuantity.QuantityValue(Quantity(value, uom))) => Some((uom.toString, value))
    case List(Percentage(pc)) => Some(("%", pc))
    case other => log.debug("Couldn't extract: %s, key: %s" % (other, subTypeKey)); None
  }

  def extractValues(marketData: MarketData): Iterable[(MarketDataValueKey, String, Double)] = subTypeKey.castRows(marketData)
    .flatMap(row => extractValue(row).map { case (uom, value) => (subTypeKey.valueKey(row), uom, value) })

  val extendedKey = MarketDataExtendedKey(-1, marketDataSet, subTypeKey.dataType, observationPoint.timeOfDay, subTypeKey)
}

object MarketDataID {
  def apply(timedKey: TimedMarketDataKey, marketDataSet: MarketDataSet) =
    new MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key)
}