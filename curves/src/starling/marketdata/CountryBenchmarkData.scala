package starling.marketdata

import starling.market.Commodity
import starling.pivot._
import model.UndefinedValue
import pivotparsers.{DayPivotParser}
import starling.quantity.Quantity
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._
import starling.daterange.{Tenor, ObservationTimeOfDay, Day}
import utils.TenorPivotParser

case class CountryBenchmarkData(countryData: NestedMap[(NeptuneCountryCode, GradeCode), Tenor, Quantity]) extends MarketData {
  def size = countryData.nestedSize
}

class CountryBenchmarkDataType(referenceData: ReferenceDataLookup = ReferenceDataLookup.Null) extends MarketDataType {
  type dataType = CountryBenchmarkData
  type keyType = CountryBenchmarkMarketDataKey
  val humanName = "country benchmarks"

  val commodityField = FieldDetails("Commodity", new SpecifiedValuesParser(Commodity.metalsCommodities.map(_.name).toSet))
  val areaField = FieldDetails("Area")
  val countryCodeField = FieldDetails.coded("Country", referenceData.countries.values)
  val gradeCodeField = FieldDetails.coded("Grade", referenceData.grades.values)
  val effectiveMonthField = FieldDetails("Effective Month", TenorPivotParser)
  val benchmarkPriceField = FieldDetails.createMeasure("Benchmark Price",
    parser0 = PricePivotParser, formatter0 = PivotQuantitySetPivotFormatter)

  def extendedKeys = List(commodityField)
  override def valueKeys = List(countryCodeField, gradeCodeField, effectiveMonthField)
  override def derivedFieldDetails = List(areaField)
  def valueFieldDetails = List(benchmarkPriceField)

  val initialPivotState = PivotFieldsState(
    dataFields = List(benchmarkPriceField.field),
    rowFields = List(commodityField.field, countryCodeField.field, gradeCodeField.field, effectiveMonthField.field),
    filters = List(
      Field("Observation Day") -> SomeSelection(Set(UndefinedValue)),
      Field("Observation Time") -> SomeSelection(Set(ObservationTimeOfDay.RealTime.name))
    )
  )

  def createKey(row: Row) = CountryBenchmarkMarketDataKey(Commodity.fromName(row.string(commodityField)))

  def createValue(rows: List[Row]) = {
    val data = rows.map { row =>
      (NeptuneCountryCode(row.string(countryCodeField)), GradeCode(row.string(gradeCodeField))) → (row[Tenor](effectiveMonthField), row.quantity(benchmarkPriceField))
    }

    CountryBenchmarkData(data.toNestedMap)
  }

  protected def fieldValuesFor(key: CountryBenchmarkMarketDataKey) = Row(commodityField.field → key.commodity.name)

  def rows(key: CountryBenchmarkMarketDataKey, data: CountryBenchmarkData) = data.countryData.mapNested {
    case ((countryCode, gradeCode), effectiveMonth, price) => Row(
      commodityField.field → key.commodity.name,
      countryCodeField.field → countryCode.code,
      gradeCodeField.field → gradeCode.code,
      areaField.field → referenceData.areaFor(countryCode).map(_.name).getOrElse("Unknown"),
      effectiveMonthField.field → effectiveMonth,
      benchmarkPriceField.field → price.pq
    )
  }
}

case class CountryBenchmarkMarketDataKey(commodity: Commodity) extends MarketDataKey {
  type marketDataType = CountryBenchmarkData
  type marketDataDBType = CountryBenchmarkData
  def typeName = MarketDataTypeName("CountryBenchmark")
  def humanName = commodity.toString
  def fields = Set(Field("Commodity"))
}

// Prefixing these classes with 'Neptune' because they aren't really countries, they are Country + Location
case class NeptuneCountryCode(code: String) {
  override def toString = code
}
case class NeptuneCountry(code: NeptuneCountryCode, name: String, area: Option[Area]) extends Tupleable {
  def tuple = (code.code, name)
  override def toString = name
}
