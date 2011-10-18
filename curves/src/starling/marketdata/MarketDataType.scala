package starling.marketdata

import starling.curves.SpreadStdDevSurfaceDataType
import starling.pivot.{Row, Field, PivotFieldsState, FieldDetails}
import starling.utils.ImplicitConversions._

/**
 * There is one of these for each time of market data. eg. prices, spotfx, forward rates...
 */
trait MarketDataType {
  type dataType <: MarketData
  type keyType <: MarketDataKey

  val name: MarketDataTypeName = MarketDataTypeName(
    getClass.getName.substring(getClass.getName.lastIndexOf(".") + 1).stripSuffix("$").stripSuffix("DataType")
  )

  // Fields needed to uniquely define a MarketDataKey. For prices it would be market
  def marketDataKeyFields: Set[Field]

  // Fields needed to uniquely define some market datum. For prices it would be market and period.
  def keyFields: Set[Field]

  // The field (always one?) for the market data - e.g. price
  def valueFields: List[Field] // TODO [08 Jun 2011] Shouldn't valueFields be everything other than the keyFields ?
  def zeroFields: List[Field] = valueFields
  def createKey(row: Row): MarketDataKey

  /**Creates a market data type from the list of field values. Typically these
   * contain user overrides and data from Lim
   */
  def createValue(rows: List[Row]): dataType

  def getValues(row: Row): List[Any] = valueFields.toList.flatMap(row.get[Any](_))

  override val toString = name.name

  //The fields to show in the market data viewer when pivoting on this type of market data
  //must be consistent with the rows method in the associated MarketDataKey
  val fields: List[FieldDetails]

  //The initial state to use in the market data viewer for this type of market data
  val initialPivotState: PivotFieldsState

  def splitByFieldType[T](map: Map[Field, T]) = map.filterKeys(keyFields) → map.filterKeys(f => !keyFields.contains(f))

  def valueKeys(key: MarketDataKey, data: MarketData) = {
    castRows(key, data).map(valueKey(_, key.fields)).toList
  }

  def fieldValues(row: Row): Row = fieldValues(createKey(row))
  def fieldValues(key: MarketDataKey): Row = fieldValuesFor(key.asInstanceOf[keyType])
  protected def fieldValuesFor(key: keyType): Row

  def valueKey(row: Row, key: MarketDataKey): MarketDataValueKey = valueKey(row, fieldValues(key).fields)

  private def valueKey(row: Row, fieldKeys: Set[Field]): MarketDataValueKey =
    MarketDataValueKey(-1, row.filterKeys(keyFields -- fieldKeys))

  def castRows(key: MarketDataKey, data: MarketData): Iterable[Row] = {
    rows(key.asInstanceOf[keyType], data.asInstanceOf[dataType])
  }

  def rows(key: keyType, data: dataType): Iterable[Row]

  val defaultValue: Row = Row()

  override def hashCode() = name.hashCode()

  override def equals(other: Any) = other match {
    case mdt: MarketDataType => name == mdt.name
    case _ => false
  }
}

case class MarketDataTypeName(name: String) {
  override def toString = name
}

class MarketDataTypes(referenceDataLookup: ReferenceDataLookup) {
  // This list is used to load MarketDataType-s from the database. (It was indirectly used to populate the drop down
  // list in the market data viewer but is reported not to be anymore).
  val types = List(
    PriceDataType,
    OilVolSurfaceDataType,
    ForwardRateDataType,
    SpotFXDataType,
    PriceFixingsHistoryDataType,
    SpreadStdDevSurfaceDataType,
    new GradeAreaBenchmarkDataType(referenceDataLookup),
    new CountryBenchmarkDataType(referenceDataLookup),
    new FreightParityDataType(referenceDataLookup)
  )

  val lookup = types.toMapWithKeys(_.name.name)

  def fromName(name: MarketDataTypeName): MarketDataType = fromName(name.name)

  def fromName(name: String): MarketDataType = lookup.getOrElse(name,
    throw new Exception("No market data type found for name: " + name + ", available: " + types.map(_.name).mkString(", ")))
}
