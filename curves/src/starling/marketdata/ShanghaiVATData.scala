package starling.marketdata

import starling.quantity.Quantity
import starling.pivot._

object ShanghaiVATDataType extends MarketDataType{
  type dataType = ShanghaiVATData
  type keyType = ShanghaiVATDataKey
  val rateField = new PivotQuantityFieldDetails("Rate")
  def rows(key : ShanghaiVATDataKey, data : ShanghaiVATData) = List(Row(
    rateField.field → PivotQuantity(data.rate)
  ))

  val initialPivotState = PivotFieldsState(
    dataFields = List(rateField.field),
    rowFields = Nil//List(nameField.field)
  )
  def createKey(row : Row) = ShanghaiVATDataKey()
  def createValue(rows : List[Row]) = ShanghaiVATData(Row.singleRow(rows, "Shanghai VAT").pivotQuantity(rateField).quantityValue.get)
  def extendedKeys = Nil
  def valueFieldDetails = List(rateField)

  protected def fieldValuesFor(key : ShanghaiVATDataKey) = Row()
}

case class ShanghaiVATDataKey() extends MarketDataKey{
  def typeName = ShanghaiVATDataType.name
  def subTypeKey = "Shanghai VAT"
  def fields = Set()
}

case class ShanghaiVATData(rate : Quantity) extends MarketData{
  def size : Int = 1
}

