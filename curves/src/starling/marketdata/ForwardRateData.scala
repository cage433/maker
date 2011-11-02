package starling.marketdata


import starling.daterange.Day
import starling.quantity.{Percentage, UOM}
import starling.utils.ImplicitConversions._

import starling.pivot._
import starling.quantity.Percentage._

object ForwardRateDataType extends MarketDataType {
  type dataType = ForwardRateData
  type keyType = ForwardRateDataKey
  val keys = UOM.currencies.map(s => ForwardRateDataKey(s))

  def extendedKeys = List(currencyField)
  override def valueKeys = List(formatField, dayField, instrumentTypeField)
  def valueFieldDetails = List(rateField)

  val currencyField = FieldDetails("Currency")
  val formatField = FieldDetails("Format")
  val instrumentTypeField = FieldDetails("Instrument Type")
  val dayField = FieldDetails("Day")
  val rateField = FieldDetails.createMeasure("Rate", PercentagePivotFormatter, PercentagePivotParser)

  def createKey(row: Row) = ForwardRateDataKey(row[UOM](currencyField))
  def createValue(rows: List[Row]) = ForwardRateData(rows.map { row =>
    ForwardRateDataEntry(row[Day](dayField), row.string(formatField), row.string(instrumentTypeField), row[Percentage](rateField))
  })

  val initialPivotState = PivotFieldsState(
    dataFields=List(rateField.field),
    rowFields=List(dayField.field),
    columnFields=List(currencyField.field)
  )

  protected def fieldValuesFor(key: ForwardRateDataKey) = Row(currencyField.field → key.ccy)

  def rows(key: ForwardRateDataKey, data: ForwardRateData) = data.entries.map { entry => Row(
    currencyField.field → key.ccy,
    formatField.field → (if (entry.format== null) "" else entry.format),
    instrumentTypeField.field → entry.trinityInstrumentType,
    dayField.field → entry.forwardDay,
    rateField.field → Percentage(entry.rate)
  ) }
}

case class ForwardRateDataEntry(forwardDay : Day, format : String, trinityInstrumentType : String, rate : Double) {
  def isDiscount = format == "Discount"
  def key() = (forwardDay, format, trinityInstrumentType)
}

case class ForwardRateDataKey(ccy : UOM) extends MarketDataKey {
  if(!ccy.isCurrency) {
    assert(ccy.isCurrency, ccy + " is not a currency")
  }
  type marketDataType = ForwardRateData
  type marketDataDBType = ForwardRateData
  def typeName = ForwardRateDataType.name
  def subTypeKey = ccy.toString
  def fields = Set(ForwardRateDataType.currencyField.field)
}

case class ForwardRateData(entries : List[ForwardRateDataEntry]) extends MarketData {
  def lastDay = entries.maximum(_.forwardDay)
  def size = entries.size
  private def toMap() = {
    entries.groupBy(_.key())
  }
  override def equals(obj: Any) = {
    obj match {
      case rhs:ForwardRateData => toMap() == rhs.toMap()
      case _ => false
    }
  }
  override def hashCode = toMap().hashCode
}



