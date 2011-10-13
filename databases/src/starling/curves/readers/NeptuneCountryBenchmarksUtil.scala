package starling.curves.readers

import starling.db.{MarketDataEntry, MarketDataSource}
import starling.dbx.QueryBuilder._
import starling.daterange.{ObservationPoint, Day}
import starling.richdb.RichDB
import starling.utils.Log

import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import collection.immutable.Map
import starling.marketdata._
import scala.collection.mutable.{Set => MSet}
import starling.market.Commodity
import starling.quantity.Quantity
import starling.market.NeptunePricingExchange
import starling.market.NeptuneCommodity


class NeptuneCountryBenchmarksUtil(neptuneDB: RichDB) extends Log {
  val refData = DBReferenceDataLookup(neptuneDB)
  def read(day: Day) = {
    val today = Day.today
    val badCommodityNames = MSet[String]()
    val badCountryCodes = MSet[String]()
    var rows = List[(NeptuneCommodity, (NeptuneCountryCode, Quantity))]()

    neptuneDB.query(
        select("material.description as material_description", "country_code", "benchmark_rate")
          from("[live].[country_mat_benchmark] country_mat_benchmark")
      leftJoin("[live].[material] material", "material.code" eql "country_mat_benchmark.material_code")) { rs =>

      val neptuneCommodityName = rs.getString("material_description")
      val commodityOption = Commodity.neptuneCommodityFromNeptuneName(neptuneCommodityName) 
      val countryCode = NeptuneCountryCode(rs.getString("country_code"))
      val exchangeOption = refData.areaFor(countryCode).flatMap{a : Area => NeptunePricingExchange.fromArea(a.code)}
      val rate = rs.getDouble("benchmark_rate")
      (commodityOption, exchangeOption) match {
        case (None, _) => badCommodityNames += neptuneCommodityName
        case (_, None) => badCountryCodes += countryCode.code
        case (Some(commodity), Some(exchange)) => {
          val price = Quantity(rate, NeptunePriceUOM(commodity, exchange))
          rows = (commodity → (countryCode, price)) :: rows
        } 
      }
    }

    if (! badCountryCodes.isEmpty)
      log.warn("Unable to import country benchmarks for countries " + badCountryCodes.mkString(", ") + ", as area missing or unrecognised")

    if (! badCommodityNames.isEmpty)
      log.warn("Unable to import country benchmarks for unrecognised commodities " + badCommodityNames.mkString(", "))

    val entries = rows.toMultiMap.map { case (neptuneCommodity, values) =>
        val data = values.map { case (countryCode, price) => countryCode → (day → price) }

        MarketDataEntry(ObservationPoint(today), neptuneCommodity.countryBenchmarkKey, CountryBenchmarkData(data.toNestedMap))
      }.toList

    Map((today, today, CountryBenchmarkDataType) → entries)
  }
}
