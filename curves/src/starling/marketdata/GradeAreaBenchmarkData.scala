package starling.marketdata

import starling.quantity.Quantity
import starling.market.Commodity
import starling.pivot._
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._

case class AreaCode(code:String)
case class Area(code:AreaCode, name:String) {
  override def toString = name
}

case class GradeCode(code : String) {
  require(code != "ReferenceDataLookup.Null")
}
case class Grade(code:GradeCode, name:String) {
  override def toString = name
}

case class Material(commodity:Commodity, grade:Grade)

/** benchmark data for (grade, area) to quantity */
case class GradeAreaBenchmarkData(areaData : Map[(GradeCode, AreaCode), Quantity]) extends MarketData {
  def size = areaData.size
}

/** Benchmark area market data key represents a list of grade, area market data rows keyed per commodity */
case class GradeAreaBenchmarkMarketDataKey(commodity : Commodity) extends MarketDataKey {
  import GradeAreaBenchmarkDataType._

  type marketDataType = GradeAreaBenchmarkData
  type marketDataDBType = GradeAreaBenchmarkData
  def dataType = GradeAreaBenchmarkDataType
  def subTypeKey = commodity.toString
  def fieldValues(referenceDataLookup: ReferenceDataLookup) = Map(commodityField.field → commodity.name)

  override def rows(marketData: GradeAreaBenchmarkData, referenceDataLookup: ReferenceDataLookup) = {
    marketData.areaData.map { case ((gradeCode, areaCode), price) => Row(
      commodityField.field → commodity.name,
      gradeField.field → referenceDataLookup.gradeFor(gradeCode),
      areaField.field → referenceDataLookup.areaFor(areaCode),
      benchmarkPriceField.field → price
    ) }
  }
}

object GradeAreaBenchmarkDataType extends MarketDataType {
  type dataType = GradeAreaBenchmarkData
  override val readonly = true

  val commodityField = FieldDetails("Commodity")
  val areaField = FieldDetails("Area")
  val gradeField = FieldDetails("Grade")
  val benchmarkPriceField = FieldDetails.createMeasure("Benchmark Price")

  def marketDataKeyFields = keyFields
  override def keyFields = Set(commodityField, areaField, gradeField).map(_.field)
  override def valueFields = List(benchmarkPriceField.field)
  val fields = List(commodityField, areaField, gradeField, benchmarkPriceField)

  val initialPivotState = PivotFieldsState(
    dataFields=List(benchmarkPriceField.field),
    rowFields=List(areaField.field, gradeField.field),
    columnFields=List(commodityField.field)
  )

  def createKey(row: Row) = GradeAreaBenchmarkMarketDataKey(Commodity.fromName(row.string(commodityField)))

  def createValue(rows: List[Row]) = {
    val data = rows.map { row => (row[Grade](gradeField).code, row[Area](areaField).code) → row.quantity(benchmarkPriceField) }

    GradeAreaBenchmarkData(data.toMap)
  }
}