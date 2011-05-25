package starling.marketdata

import starling.pivot.Field
import starling.curves.MarketDataSlice
import starling.db.MarketDataReader
import starling.daterange.ObservationPoint

/**
 * The key used to look up market data from the database.
 * Typically it is the market data type + the market/currency
 */
trait MarketDataKey {

  //the type of MarketData returned when using this key
  type marketDataType <: MarketData
  type marketDataDBType

  //the MarketDataType object for this type
  def dataType:MarketDataType

  //a string to identify this key in the database (typically the market or currency)
  def subTypeKey : String

  //represents the MarketData passed in as rows for use in the market data viewer pivot
  //the fields used as keys must be consistent with the fields defined by the associated MarketDataType
  def rows(t : marketDataType) : Iterable[Map[Field,Any]]

  def castRows(marketData:MarketData) : Iterable[Map[Field,Any]]= rows(cast(marketData))

  //def create(rows:Iterable[Map[Field,Any]]):marketDataType = throw new Exception

  //the field values for this key
  def fieldValues:Map[Field,Any]

  def read(slice:MarketDataSlice):marketDataType = cast(slice.read(this))
  def read(observationPoint: ObservationPoint, reader:MarketDataReader):marketDataType = cast(reader.read(observationPoint, this))

  private def cast(marketData:MarketData):marketDataType = marketData.asInstanceOf[marketDataType]

  def dataTypeKey = dataType.name

  def unmarshallDB(dbValue: Any): marketDataType = dbValue.asInstanceOf[marketDataType]
}
