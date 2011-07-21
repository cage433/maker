package starling.marketdata

import starling.quantity.{UOMSymbol, Quantity, UOM}
import starling.pivot._


object SpotFXDataType extends MarketDataType {
  type dataType = SpotFXData
  val keys = UOMSymbol.currencySymbols.filterNot(_ == UOMSymbol.usd).map(s=>SpotFXDataKey(s.asUOM))
  val currencyField = FieldDetails("Currency")
  val rateField = new PivotQuantityFieldDetails("Rate")
  override def createKey(values:Map[Field,Any]):MarketDataKey = SpotFXDataKey(UOM.parseCurrency(values(currencyField.field).asInstanceOf[String]).get)
  override def createValue(values:List[Map[Field,Any]]):dataType = {
    values match {
      case Nil => throw new Exception("Can't create spot fx rate from no rows")
      case one :: Nil => SpotFXData(one(rateField.field).asInstanceOf[PivotQuantity].quantityValue.get)
      case _ => throw new Exception("Can't create spot fx rate from more than one row " + values)
    }
  }

  val initialPivotState = PivotFieldsState(
    dataFields=List(rateField.field),
    rowFields=List(currencyField.field)
  )
  def marketDataKeyFelds = Set(currencyField.field)
  def keyFields = Set(currencyField.field)
  def valueFields = Set(rateField.field)
  val fields = List(currencyField, rateField)
}

/**
 * Against USD. So ccy/USD
 */
case class SpotFXDataKey(ccy: UOM) extends MarketDataKey {
  type marketDataType = SpotFXData
  type marketDataDBType = SpotFXData
  def dataType = SpotFXDataType
  def subTypeKey = ccy.toString
  override def rows(data : SpotFXData) = List(Map(
    SpotFXDataType.currencyField.field -> ccy.toString,
    SpotFXDataType.rateField.field -> PivotQuantity(data.rate)
   ))

  def fieldValues = Map(SpotFXDataType.currencyField.field -> ccy.toString)
}

//For Example 0.0125 USD / JPY
case class SpotFXData(rate: Quantity) extends MarketData {
  override def size = Some(1)
}

