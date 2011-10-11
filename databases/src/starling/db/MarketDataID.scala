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
case class MarketDataID(observationPoint: ObservationPoint, marketDataSet: MarketDataSet, subTypeKey: MarketDataKey) extends Log {
  private val noComment: String = null

  def conditions : Map[String, Any] = Map(
    "observationTime" -> observationPoint.timeName,
    "observationDay" -> observationPoint.day.getOrElse(null),
    "marketDataSet" -> marketDataSet.name,
    "marketDataType" -> StarlingXStream.write(subTypeKey.dataType),
    "marketDataKey" -> StarlingXStream.write(subTypeKey))

  def extractValue(row: Row): Option[(String, Double, String)] = subTypeKey.dataType.getValues(row) match {
    case List(Quantity(value, uom)) => Some((uom.toString, value, noComment))
    case List(pq: PivotQuantity) => pq.quantityValue.map(q => (q.uom.toString, q.value, noComment))
    case List(Percentage(pc)) => Some(("%", pc, noComment))
    case List(Quantity(value, uom), comment: String) => Some((uom.toString, value, comment))
    case other => log.warn("Couldn't extract: %s, key: %s" % (other, subTypeKey)); None
  }

  def extractValues(marketData: MarketData, referenceDataLookup: ReferenceDataLookup) = {
    subTypeKey.dataType.castRows(subTypeKey, marketData, referenceDataLookup)
      .flatMap(row => extractValue(row).map { case (uom, value, comment) => (valueKey(row, referenceDataLookup), uom, value, comment) })
  }

  val extendedKey = MarketDataExtendedKey(-1, marketDataSet, subTypeKey.dataType, observationPoint.timeOfDay, subTypeKey)

  def valueKeys(data: MarketData, referenceDataLookup: ReferenceDataLookup) = {
    subTypeKey.dataType.valueKeys(subTypeKey, data, referenceDataLookup)
  }

  private def valueKey(row: Row, referenceDataLookup: ReferenceDataLookup) =
    subTypeKey.dataType.valueKey(row, subTypeKey.fieldValues(referenceDataLookup).fields)
}

object MarketDataID {
  def apply(timedKey: TimedMarketDataKey, marketDataSet: MarketDataSet) =
    new MarketDataID(timedKey.observationPoint, marketDataSet, timedKey.key)
}