package starling.db

import starling.daterange.ObservationPoint
import starling.quantity.{Percentage, Quantity}
import starling.instrument.utils.StarlingXStream
import starling.utils.Log
import starling.utils.ImplicitConversions._
import starling.pivot.{Row, PivotQuantity}
import starling.marketdata._

/**
 * Uniquely identifies a market data object in the database. Versioning is added
 * on top of this by the VersionedDatabase mixin.
 */
case class MarketDataID(observationPoint: ObservationPoint, marketDataSet: MarketDataSet, subTypeKey: MarketDataKey, types: MarketDataTypes) extends Log {
  private val noComment: String = null

  def conditions : Map[String, Any] = Map(
    "observationTime" -> observationPoint.timeName,
    "observationDay" -> observationPoint.day.getOrElse(null),
    "marketDataSet" -> marketDataSet.name,
    "marketDataType" -> StarlingXStream.write(subTypeKey.typeName),
    "marketDataKey" -> StarlingXStream.write(subTypeKey))

  def extractValue(row: Row): Option[(String, Double, String)] = types.fromName(subTypeKey.typeName).getValues(row) match {
    case List(Quantity(value, uom)) => Some((uom.toString, value, noComment))
    case List(pq: PivotQuantity) => pq.quantityValue.map(q => (q.uom.toString, q.value, noComment))
    case List(Percentage(pc)) => Some(("%", pc, noComment))
    case List(Quantity(value, uom), comment: String) => Some((uom.toString, value, comment))
    case other => log.warn("Couldn't extract: %s, key: %s" % (other, subTypeKey)); None
  }

  def extractValues(marketData: MarketData) = types.fromName(subTypeKey.typeName).castRows(subTypeKey, marketData)
    .flatMap(row => extractValue(row).map { case (uom, value, comment) => (valueKey(row), uom, value, comment) })

  val extendedKey = MarketDataExtendedKey(-1, marketDataSet, types.fromName(subTypeKey.typeName), observationPoint.timeOfDay, subTypeKey)

  def valueKeys(data: MarketData) = types.fromName(subTypeKey.typeName).valueKeys(subTypeKey, data)

  private def valueKey(row: Row) = types.fromName(subTypeKey.typeName).valueKey(row, subTypeKey)
}

object MarketDataID {
  def apply(timedKey: TimedMarketDataKey, marketDataSet: MarketDataSet, types: MarketDataTypes) =
    new MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key, types)
}