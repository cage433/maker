package starling.marketdata

import starling.daterange.Day
import starling.market.Commodity
import starling.pivot._
import starling.quantity.Quantity
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._

case class CountryBenchmarkData(countryData: NestedMap[NeptuneCountryCode, Day, Quantity]) extends MarketData {
  def size = countryData.nestedSize
}

object CountryBenchmarkDataType extends MarketDataType {
  type dataType = CountryBenchmarkData
  override val readonly = true

  val commodityField = FieldDetails("Commodity")
  val countryField = FieldDetails("Country")
  val effectiveFromField = FieldDetails("Effective From")
  val benchmarkPriceField = FieldDetails.createMeasure("Benchmark Price")

  def marketDataKeyFields = keyFields
  override def keyFields = Set(commodityField, countryField).map(_.field)
  override def valueFields = List(benchmarkPriceField.field)
  val fields = List(commodityField, countryField, benchmarkPriceField)

  val initialPivotState = PivotFieldsState(
    dataFields = List(benchmarkPriceField.field),
    rowFields = List(countryField.field),
    columnFields = List(commodityField.field)
  )

  def createKey(row: Row) = CountryBenchmarkMarketDataKey(Commodity.fromName(row.string(commodityField)))

  def createValue(rows: List[Row]) = {
    val data = rows.map { row =>
      row[NeptuneCountry](countryField).code → (row[Day](effectiveFromField), row.quantity(benchmarkPriceField))
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
      countryField.field → referenceDataLookup.countryFor(countryCode),
      effectiveFromField.field → effectiveFromField,
      benchmarkPriceField.field → price
    ) }
  }
}

// Prefixing these classes with 'Neptune' because they aren't really countries, they are Country + Location
case class NeptuneCountryCode(code: String)
case class NeptuneCountry(code: NeptuneCountryCode, name: String, area: Option[Area]) {
  override def toString = name
}