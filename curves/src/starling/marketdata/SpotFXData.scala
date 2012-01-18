package starling.marketdata

import starling.quantity.{UOMSymbol, Quantity, UOM}
import starling.pivot._
import starling.utils.ImplicitConversions._


object SpotFXDataType extends MarketDataType {
  type dataType = SpotFXData
  type keyType = SpotFXDataKey
  val humanName = "spot fx rate"
  val keys = UOM.currencies.filterNot(_ == UOM.USD).map(s=>SpotFXDataKey(s))
  val currencyField = FieldDetails("Currency")
  val rateField = new PivotQuantityFieldDetails("Rate")
  def createKey(row: Row) = SpotFXDataKey(UOM.parseCurrency(row.string(currencyField)))
  def createValue(rows: List[Row]) = SpotFXData(Row.singleRow(rows, "spot fx rate").pivotQuantity(rateField).quantityValue.get)

  def extendedKeys = List(currencyField)
  def valueFieldDetails = List(rateField)

  val initialPivotState = PivotFieldsState(
    dataFields=List(rateField.field),
    rowFields=List(currencyField.field, Field("Observation Time"))
  )

  def rows(key: SpotFXDataKey, data: SpotFXData) = List(Row(
    currencyField.field → key.ccy.toString,
    rateField.field → PivotQuantity(data.rate)
  ))

  protected def fieldValuesFor(key: SpotFXDataKey) = Row(currencyField.field → key.ccy.toString)
}

/**
 * Against USD. So ccy/USD
 */
case class SpotFXDataKey(ccy: UOM) extends MarketDataKey {
  type marketDataType = SpotFXData
  def typeName = SpotFXDataType.name
  def humanName = ccy.toString
  def fields = Set(SpotFXDataType.currencyField.field)
}

//For Example 0.0125 USD / JPY
case class SpotFXData(rate: Quantity) extends MarketData {
  override def size = 1
}

