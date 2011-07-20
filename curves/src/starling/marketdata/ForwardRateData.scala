package starling.marketdata


import starling.daterange.Day
import starling.quantity.{Percentage, UOMSymbol, UOM}
import starling.utils.ImplicitConversions._

import starling.pivot._

object ForwardRateDataType extends MarketDataType {
  type dataType = ForwardRateData
  val keys = UOMSymbol.currencySymbols.map(s => ForwardRateDataKey(s.asUOM))

  def marketDataKeyFelds = Set(currencyField.field)
  override def keyFields:Set[Field] = Set(currencyField.field, formatField.field, dayField.field, instrumentTypeField.field)
  override def valueFields:Set[Field] = Set(rateField.field, discountField.field)

  val currencyField = FieldDetails("Currency")
  val formatField = FieldDetails("Format")
  val instrumentTypeField = FieldDetails("Instrument Type")
  val dayField = FieldDetails("Day")
  val rateField = FieldDetails.createMeasure("Rate")
  val discountField = FieldDetails.createMeasure("Discount")


  def createKey(values: Map[Field, Any]) = ForwardRateDataKey(values(currencyField.field).asInstanceOf[UOM])
  def createValue(values: List[Map[Field, Any]]) = ForwardRateData(values.map { x => {
    ForwardRateDataEntry(
      x(dayField.field).asInstanceOf[Day],
      x(formatField.field).asInstanceOf[String],
      x(instrumentTypeField.field).asInstanceOf[String],
      x.get(rateField.field).map(_.asInstanceOf[Percentage].value).getOrElse(x(discountField.field).asInstanceOf[Double])
    )
  }})

  val initialPivotState = PivotFieldsState(
    dataFields=List(discountField.field),
    rowFields=List(dayField.field),
    columnFields=List(currencyField.field)
  )

  val fields = List(currencyField, formatField, instrumentTypeField, dayField, rateField, discountField)
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

  override def rows(data : ForwardRateData) = data.entries.map { entry =>
    val rateField = if (entry.isDiscount) {
      ForwardRateDataType.discountField.field -> entry.rate
    } else {
      ForwardRateDataType.rateField.field -> Percentage(entry.rate)
    }

    Map(
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
}



