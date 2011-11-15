package starling.marketdata

import starling.utils.ImplicitConversions._

import starling.pivot._
import starling.quantity.{Quantity, UOM}
import starling.daterange.{Day, Tenor}
import scalaz.NewType
import utils.TenorPivotParser


object ForwardRateDataType extends MarketDataType {
  type dataType = ForwardRateData
  type keyType = ForwardRateDataKey
  val keys = UOM.currencies.map(s => ForwardRateDataKey(s))
  val humanName = "forward rates"

  def extendedKeys = List(currencyField)
  override def valueKeys = List(sourceField, tenorField)
  def valueFieldDetails = List(rateField)

  val sourceField = FieldDetails("Source")
  val currencyField = FieldDetails("Currency")
  val tenorField = new FieldDetails("Tenor") {
    override def parser = TenorPivotParser
    override def comparator = new OrderedOrdering[Tenor].untyped
  }
  val rateField = FieldDetails.createMeasure("Rate", PivotQuantitySetPivotFormatter, PivotQuantityPivotParser)

  def createKey(row: Row) = ForwardRateDataKey(row[UOM](currencyField))

  def createValue(rows: List[Row]) = ForwardRateData(rows.map { row =>
    (ForwardRateSource(row.string(sourceField)), (Tenor.parse(row(tenorField)), row[Quantity](rateField)))
  }.toNestedMap)

  val initialPivotState = PivotFieldsState(
    rowFields = List(sourceField.field, currencyField.field),
    columnFields = List(tenorField.field),
    dataFields = List(rateField.field)
  )

  protected def fieldValuesFor(key: ForwardRateDataKey) = Row(currencyField.field → key.ccy)

  def rows(key: ForwardRateDataKey, data: ForwardRateData) = data.rates.mapNested { case (source, tenor, rate) =>
    fieldValuesFor(key) + Row(sourceField.field → source.value, tenorField.field → tenor, rateField.field → rate.pq)
  }
}

case class ForwardRateDataKey(ccy : UOM) extends MarketDataKey {
  ccy.ensuring(_.isCurrency, ccy + " is not a currency")

  type marketDataType = ForwardRateData
  type marketDataDBType = ForwardRateData
  def typeName = ForwardRateDataType.name
  def humanName = ccy.toString
  def fields = Set(ForwardRateDataType.currencyField.field)
}

case class ForwardRateSource(value: String) extends NewType[String]

object ForwardRateSource {
  val LIBOR = ForwardRateSource("LIBOR")
}

case class ForwardRateData(rates: NestedMap[ForwardRateSource, Tenor, Quantity]) extends MarketData {
  require(rates.keySet.forall(_.isInstanceOf[ForwardRateSource]))
  require(rates.flipNesting.keySet.forall(_.isInstanceOf[Tenor]))
  require(rates.allValues.forall(_.isInstanceOf[Quantity]))

  def lastDay(fixingDay: Day): Day = fixingDay

  def size = rates.nestedSize
}