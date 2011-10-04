package starling.marketdata

import starling.pivot.{Field, PivotFieldsState, FieldDetails}
import starling.market.EquityPricesDataType
import starling.curves.SpreadStdDevSurfaceDataType

/**
 * There is one of these for each time of market data. eg. prices, spotfx, forward rates...
 */
trait MarketDataType {

  type dataType <: MarketData

  val name: String = getClass.getName.substring(getClass.getName.lastIndexOf(".") + 1).stripSuffix("DataType$")

  // Fields needed to uniquely define a MarketDataKey. For prices it would be market
  def marketDataKeyFelds: Set[Field]

  // Fields needed to uniquely define some market datum. For prices it would be market and period.
  def keyFields: Set[Field]

  // The field (always one?) for the market data - e.g. price
  def valueFields: Set[Field] // TODO [08 Jun 2011] Shouldn't valueFields be everything other than the keyFields ?
  def createKey(values: Map[Field, Any]): MarketDataKey

  /**Creates a market data type from the list of field values. Typically these
   * contain user overrides and data from Lim
   */
  def createValue(values: List[Map[Field, Any]]): dataType

  def getValues(row: Map[Field, Any]): List[Any] = valueFields.toList.flatMap(row.get(_))

  override val toString = name

  //The fields to show in the market data viewer when pivoting on this type of market data
  //must be consistent with the rows method in the associated MarketDataKey
  val fields: List[FieldDetails]

  //The initial state to use in the market data viewer for this type of market data
  val initialPivotState: PivotFieldsState

  def splitByFieldType[T](map: Map[Field, T]) = map.filterKeys(keyFields) → map.filterKeys(f => !keyFields.contains(f))
}

object MarketDataTypes {
  // This list is used to load MarketDataType-s from the database. (It was indirectly used to populate the drop down
  // list in the market data viewer but is reported not to be anymore).
  val types = List(
    GradeHubBenchmarkDataType,
    PriceDataType,
    BradyFXVolSurfaceDataType,
    BradyMetalVolsDataType,
    OilVolSurfaceDataType,
    ForwardRateDataType,
    SpotFXDataType,
    PriceFixingsHistoryDataType,
    SpreadStdDevSurfaceDataType,
    EquityPricesDataType,
    ImpliedVolDataType,
    BenchmarkLocationMarketDataType
  )

  def fromName(name: String) = types.find(_.name == name).getOrElse(
    throw new Exception("No market data type found for name: " + name + ", available: " + types.map(_.name).mkString(", ")))
}
