package starling.marketdata

import starling.daterange.Day
import starling.market.Commodity
import starling.pivot._
import pivotparsers.DayPivotParser
import starling.quantity.Quantity
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._

case class CountryBenchmarkData(countryData: NestedMap[NeptuneCountryCode, Day, Quantity]) extends MarketData {
  def size = countryData.nestedSize
}

object CountryBenchmarkDataType extends MarketDataType {
  type dataType = CountryBenchmarkData

  val commodityField = FieldDetails("Commodity", FixedPivotParser(Commodity.metalsCommodities.map(_.name).toSet))
  val countryField = FieldDetails("Country")
  val countryCodeField = FieldDetails("Country Code")
  val effectiveFromField = FieldDetails("Effective From", DayPivotParser)
  val benchmarkPriceField = FieldDetails.createMeasure("Benchmark Price", parser0 = PivotQuantityPivotParser)

  def marketDataKeyFields = keyFields
  override def keyFields = Set(commodityField, countryCodeField, effectiveFromField).map(_.field)
  override def valueFields = List(benchmarkPriceField.field)
  val fields = List(commodityField, countryField, countryCodeField, effectiveFromField, benchmarkPriceField)

  val initialPivotState = PivotFieldsState(
    dataFields = List(benchmarkPriceField.field),
    rowFields = List(countryField.field),
    columnFields = List(commodityField.field)
  )

  def createKey(row: Row) = CountryBenchmarkMarketDataKey(Commodity.fromName(row.string(commodityField)))

  def createValue(rows: List[Row]) = {
    val data = rows.map { row =>
      NeptuneCountryCode(row.string(countryCodeField)) → (row[Day](effectiveFromField), row.quantity(benchmarkPriceField))
    }

    CountryBenchmarkData(data.toNestedMap)
  }
}

case class CountryBenchmarkMarketDataKey(commodity: Commodity) extends MarketDataKey {
  import CountryBenchmarkDataType._

  type marketDataType = CountryBenchmarkData
  type marketDataDBType = CountryBenchmarkData
  def dataType = CountryBenchmarkDataType
  def subTypeKey = commodity.toString
  def fieldValues(referenceDataLookup: ReferenceDataLookup) = Map(commodityField.field → commodity.name)

  override def rows(marketData: CountryBenchmarkData, referenceDataLookup: ReferenceDataLookup) = {
    marketData.countryData.mapNested { case (countryCode, effectiveFrom, price) => Row(
      commodityField.field → commodity.name,
      countryCodeField.field → countryCode.code,
      countryField.field → referenceDataLookup.countryFor(countryCode).name,
      effectiveFromField.field → effectiveFrom,
      benchmarkPriceField.field → price
    ) }
  }
}

// Prefixing these classes with 'Neptune' because they aren't really countries, they are Country + Location
case class NeptuneCountryCode(code: String)
case class NeptuneCountry(code: NeptuneCountryCode, name: String, area: Option[Area]) {
  override def toString = name
}