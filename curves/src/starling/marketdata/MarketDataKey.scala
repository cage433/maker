package starling.marketdata

import starling.curves.MarketDataSlice
import starling.db.MarketDataReader
import scalaz.Scalaz._
import starling.pivot.{Row, Field}
import starling.daterange.{Day, ObservationPoint}

/**
 * The key used to look up market data from the database.
 * Typically it is the market data type + the market/currency
 */
trait MarketDataKey {
  //the type of MarketData returned when using this key
  type marketDataType <: MarketData
  type marketDataDBType

  //the MarketDataType object for this type
  def typeName:MarketDataTypeName

  //the string used in error messages
  def humanName : String

  //represents the MarketData passed in as rows for use in the market data viewer pivot
  //the fields used as keys must be consistent with the fields defined by the associated MarketDataType
  // The fields include those in field values, plus market data fields. E.g. for price data the market
  // data fields are Month, price
//  final def rows(t : marketDataType, referenceDataLookup: ReferenceDataLookup): Iterable[Row] = throw new Exception("Replace")

  //def create(rows:Iterable[Map[Field,Any]]):marketDataType = throw new Exception

  //the field values for this key - everything except the market data values.
  // E.g. for price data this would have commodity, market, Exchange
  // Used as an optimisation. The gui first gets the markets from this function to display in
  // the field chooser - base on the choice 'rows' is called to get the actual market data to display.
  def fields: Set[Field]

  def read(slice:MarketDataSlice):marketDataType = cast(slice.read(this))
  def read(observationPoint: ObservationPoint, reader: MarketDataReader): marketDataType =
    cast(reader.read(TimedMarketDataKey(observationPoint, this)))

  private def cast(marketData:MarketData):marketDataType = marketData.asInstanceOf[marketDataType]

  def unmarshallDB(dbValue: Any): marketDataType = dbValue.asInstanceOf[marketDataType]
}

case class TimedMarketDataKey(observationPoint: ObservationPoint, key: MarketDataKey) {
  def day = observationPoint.day
  def timeOfDay = observationPoint.timeOfDay
  def timeShortName = observationPoint.timeOfDay.shortName
  def timeName = observationPoint.timeOfDay.name

  def typeName = key.typeName
  def fieldValues(marketDataType: MarketDataType): Row = marketDataType.fieldValues(key)

  def unmarshallDB(dbValue: Any) = key.unmarshallDB(dbValue)

  def asTuple = (observationPoint, key)
  def copyDay(other: Day) = copy(observationPoint.copyDay(other))
}

trait DBKey[K <: DBKey[K]] {
  val id: Int
  def sameValuesAs(that: K): Boolean
}

case class MarketDataValueKey(id: Int, row: Row) extends DBKey[MarketDataValueKey] {
  lazy val dbMap: Map[String, Any] = Map("valueKey" → row.dbValue)
  def sameValuesAs(that: MarketDataValueKey) = that.copy(id = id) == this
  def fields = row.fields
}

object MarketDataValueKey {
  def apply(dbMap: Map[String, Any], id: Int = -1): MarketDataValueKey = MarketDataValueKey(id, Row.create(dbMap))
}