package starling.marketdata

import starling.quantity.Quantity
import starling.market.Commodity
import starling.pivot._
import scalaz.Scalaz._

/**
 * benchmark data for 'neptune country' to quantity
 */
case class CountryBenchmarkData(countryData : List[(NeptuneCountryCode, Quantity)]) extends MarketData {
  def size = countryData.size
}

object CountryBenchmarkDataType extends MarketDataType {
  type dataType = CountryBenchmarkData
  override val readonly = true

  val commodityField = FieldDetails("Commodity")
  val countryField = FieldDetails("Country")
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
    val data = rows.map { row => (row[NeptuneCountry](countryField).code, row.quantity(benchmarkPriceField)) }

    CountryBenchmarkData(data)
  }
}

/**
 * Benchmark area market data key represents a list of grade, location market data rows keyed per commodity
 */
case class CountryBenchmarkMarketDataKey(commodity : Commodity) extends MarketDataKey {
  import CountryBenchmarkDataType._

  type marketDataType = CountryBenchmarkData
  type marketDataDBType = CountryBenchmarkData
  def dataType = CountryBenchmarkDataType
  def subTypeKey = commodity.toString
  def fieldValues = Map(commodityField.field -> commodity.name)

  override def rows(marketData: CountryBenchmarkData, referenceDataLookup: ReferenceDataLookup) = {
    marketData.countryData.map { case (countryCode, price) => Row(
      commodityField.field -> commodity.name,
      countryField.field -> referenceDataLookup.countryFor(countryCode),
      benchmarkPriceField.field -> price
    ) }
  }
}

// Prefixing these classes with 'Neptune' because they aren't really countries, they are Country + Location
case class NeptuneCountryCode(code: String)
case class NeptuneCountry(code: NeptuneCountryCode, name: String, area: Area) {
  override def toString = name + "(" + area.name + ")"
}