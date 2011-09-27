package starling.marketdata


import starling.daterange.Day
import starling.quantity.{Percentage, UOMSymbol, UOM}
import starling.utils.ImplicitConversions._

import starling.pivot._

object ForwardRateDataType extends MarketDataType {
  type dataType = ForwardRateData
  val keys = UOM.currencies.map(s => ForwardRateDataKey(s))

  def marketDataKeyFields = Set(currencyField.field)
  override def keyFields:Set[Field] = Set(currencyField.field, formatField.field, dayField.field, instrumentTypeField.field)
  override def valueFields = List(rateField.field)

  val currencyField = FieldDetails("Currency")
  val formatField = FieldDetails("Format")
  val instrumentTypeField = FieldDetails("Instrument Type")
  val dayField = FieldDetails("Day")
  val rateField = FieldDetails.createMeasure("Rate")

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
}

case class ForwardRateDataEntry(forwardDay : Day, format : String, trinityInstrumentType : String, rate : Double) {
  def isDiscount = format == "Discount"
}

case class ForwardRateDataKey(ccy : UOM) extends MarketDataKey {
  if(!ccy.isCurrency) {
    assert(ccy.isCurrency, ccy + " is not a currency")
  }
  type marketDataType = ForwardRateData
  type marketDataDBType = ForwardRateData
  def dataType = ForwardRateDataType
  def subTypeKey = ccy.toString

  override def rows(data : ForwardRateData, referenceDataLookup: ReferenceDataLookup) = data.entries.map { entry =>
    val rateField = ForwardRateDataType.rateField.field -> Percentage(entry.rate)

    Row(
        ForwardRateDataType.currencyField.field -> ccy,
        ForwardRateDataType.formatField.field -> (if (entry.format== null) "" else entry.format),
        ForwardRateDataType.instrumentTypeField.field -> entry.trinityInstrumentType,
        ForwardRateDataType.dayField.field -> entry.forwardDay,
        rateField
    )
  }
  def fieldValues = Map(ForwardRateDataType.currencyField.field -> ccy)
}

case class ForwardRateData(entries : List[ForwardRateDataEntry]) extends MarketData {
  def lastDay = entries.maximum(_.forwardDay)
  def size = entries.size
}



