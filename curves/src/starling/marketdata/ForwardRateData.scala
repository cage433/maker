package starling.marketdata


import starling.daterange.Day
import starling.quantity.{Percentage, UOMSymbol, UOM}
import starling.utils.ImplicitConversions._

import starling.pivot._
import starling.quantity.Percentage._

object ForwardRateDataType extends MarketDataType {
  type dataType = ForwardRateData
  type keyType = ForwardRateDataKey
  val keys = UOM.currencies.map(s => ForwardRateDataKey(s))

  def marketDataKeyFields = Set(currencyField.field)
  override def keyFields:Set[Field] = Set(currencyField.field, formatField.field, dayField.field, instrumentTypeField.field)
  override def valueFields = List(rateField.field)

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

  val fields = List(currencyField, formatField, instrumentTypeField, dayField, rateField)

  def rows(key: ForwardRateDataKey, data: ForwardRateData) = data.entries.map { entry =>
    Row(
        ForwardRateDataType.currencyField.field → key.ccy,
        ForwardRateDataType.formatField.field → (if (entry.format== null) "" else entry.format),
        ForwardRateDataType.instrumentTypeField.field → entry.trinityInstrumentType,
        ForwardRateDataType.dayField.field → entry.forwardDay,
        ForwardRateDataType.rateField.field → Percentage(entry.rate)
    )
  }
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
  def dataTypeName = ForwardRateDataType.name
  def subTypeKey = ccy.toString
  def fieldValues(referenceDataLookup: ReferenceDataLookup) = Row(ForwardRateDataType.currencyField.field → ccy)
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



