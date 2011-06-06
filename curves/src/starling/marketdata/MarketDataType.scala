package starling.marketdata

import starling.pivot.{Field, SomeSelection, PivotFieldsState, FieldDetails}
import starling.curves.SpreadStdDevSurfaceDataType
import starling.market.EquityPricesDataType

/**
 * There is one of these for each time of market data. eg. prices, spotfx, forward rates...
 */
trait MarketDataType {
  type dataType <: MarketData

  val name: String = {
    val className = getClass.getName.substring(getClass.getName.lastIndexOf(".") + 1)
    className.substring(0, className.length-"DataType$".length)
  }
  def keyFields:Set[Field]
  def valueFields:Set[Field]
  def createKey(values:Map[Field,Any]):MarketDataKey
  def createValue(values:List[Map[Field,Any]]):dataType
  override val toString = name

  //The fields to show in the market data viewer when pivoting on this type of market data
  //must be consistent with the rows method in the associated MarketDataKey
  val fields:List[FieldDetails]

  //The initial state to use in the market data viewer for this type of market data
  val initialPivotState:PivotFieldsState
}

object MarketDataTypes {
  //This list is indirectly used to populate the drop down list in the market data viewer
  val types = List(
    PriceDataType,
    BradyFXVolSurfaceDataType,
    BradyMetalVolsDataType,
    OilVolSurfaceDataType,
    ForwardRateDataType,
    SpotFXDataType,
    PriceFixingsHistoryDataType,
    SpreadStdDevSurfaceDataType,
    EquityPricesDataType)

  def fromName(name: String) = types.find(_.name == name).getOrElse(
    throw new Exception("No market data type found for name: " + name + ", available: " + types.map(_.name).mkString(", ")))
}

