package starling.marketdata

import starling.quantity.Quantity
import starling.market.Commodity
import starling.pivot._
import pivotparsers.{SpecifiedValuesParser, DayPivotParser}
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._
import starling.daterange.Day

case class AreaCode(code:String) {
  override def toString = code
}
object AreaCode{
  val EUR = AreaCode("EUR")
  val SAM = AreaCode("SAM")
  val NAM = AreaCode("NAM")
  val ASI = AreaCode("ASI")
  val CHN = AreaCode("CHN")
  val hardCoded = Set(EUR, SAM, NAM, ASI, CHN)
}
case class Area(code:AreaCode, name:String) {
  override def toString = name
}

case class GradeCode(code : String) {
  override def toString = code
}
case class Grade(code:GradeCode, name:String) {
  override def toString = name
}

/** benchmark data for (grade, area) to quantity */
case class GradeAreaBenchmarkData(areaData : NestedMap[(GradeCode, AreaCode), Day, Quantity]) extends MarketData {
  def size = areaData.nestedSize
}

/** Benchmark area market data key represents a list of grade, area market data rows keyed per commodity */
case class GradeAreaBenchmarkMarketDataKey(commodity : Commodity) extends MarketDataKey {
  type marketDataType = GradeAreaBenchmarkData
  type marketDataDBType = GradeAreaBenchmarkData
  def typeName = MarketDataTypeName("GradeAreaBenchmark")
  def subTypeKey = commodity.toString
  def fields = Set(Field("Commodity"))
}

class GradeAreaBenchmarkDataType(referenceData: ReferenceDataLookup = ReferenceDataLookup.Null) extends MarketDataType {
  type dataType = GradeAreaBenchmarkData
  type keyType = GradeAreaBenchmarkMarketDataKey

  val commodityField = FieldDetails("Commodity", new SpecifiedValuesParser(Commodity.metalsCommodities.map(_.name).toSet))
  val areaField = FieldDetails("Area")
  val areaCodeField = FieldDetails("Area Code", new SpecifiedValuesParser(referenceData.areas.keySet.map(_.code)))
  val gradeField = FieldDetails("Grade")
  val gradeCodeField = FieldDetails("Grade Code", new SpecifiedValuesParser(referenceData.grades.keySet.map(_.code)))
  val effectiveFromField = FieldDetails("Effective From", DayPivotParser)
  val benchmarkPriceField = FieldDetails.createMeasure("Benchmark Price",
    parser0 = PivotQuantityPivotParser, formatter0 = PivotQuantitySetPivotFormatter)

  def extendedKeys = List(commodityField)
  override def valueKeys = List(areaCodeField, gradeCodeField, effectiveFromField)
  override def derivedFieldDetails = List(areaField, gradeField)
  def valueFieldDetails = List(benchmarkPriceField)

  val initialPivotState = PivotFieldsState(
    dataFields = List(benchmarkPriceField.field),
    rowFields = List(areaCodeField, commodityField, gradeCodeField, effectiveFromField).map(_.field),
    filters = List( Field("Observation Time") -> SomeSelection(Set("Default")) )
  )

  def createKey(row: Row) = GradeAreaBenchmarkMarketDataKey(Commodity.fromName(row.string(commodityField)))

  def createValue(rows: List[Row]) = {
    val data = rows.map { row => (GradeCode(row.string(gradeCodeField)), AreaCode(row.string(areaCodeField))) →
      (row[Day](effectiveFromField), row.quantity(benchmarkPriceField))
    }

    GradeAreaBenchmarkData(data.toNestedMap)
  }

  protected def fieldValuesFor(key: GradeAreaBenchmarkMarketDataKey) = Row(commodityField.field → key.commodity.name)

  def rows(key: GradeAreaBenchmarkMarketDataKey, data: GradeAreaBenchmarkData) = data.areaData.mapNested {
    case ((gradeCode, areaCode), effectiveFrom, price) => Row(
      commodityField.field → key.commodity.name,
      gradeCodeField.field → gradeCode.code,
      gradeField.field → referenceData.gradeFor(gradeCode).name,
      areaCodeField.field → areaCode.code,
      areaField.field → referenceData.areaFor(areaCode).name,
      effectiveFromField.field → effectiveFrom,
      benchmarkPriceField.field → price.pq
    )
  }
}
