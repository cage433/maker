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


class NeptuneCountryBenchmarksMarketDataSource(neptuneDB: RichDB) extends MarketDataSource with Log {
  def read(day: Day) = Map((day, day, CountryBenchmarkDataType) → readCountryBenchmarks(day))

  def readCountryBenchmarks(day:Day):List[MarketDataEntry] = {
    val (rows, invalidRows) = neptuneDB.queryWithResult(
        select("material.description as material_description", "country_code", "benchmark_rate")
          from("[live].[country_mat_benchmark] country_mat_benchmark")
      leftJoin("[live].[material] material", "material.code" eql "country_mat_benchmark.material_code")) { rs =>

      NeptuneCommodity(rs.getString("material_description")) →
        (NeptuneCountryCode(rs.getString("country_code")), rs.getDouble("benchmark_rate"))
    }.toMultiMap.partitionKeys(_.isValid)

    invalidRows.ifDefined { logInvalid("Cannot import contry benchmarks for neptune commodities: ", _) }

    rows.map { case (neptuneCommodity, values) =>
      val data = values.map { case (countryCode, price) => countryCode → neptuneCommodity.toQuantity(price) }

      MarketDataEntry(ObservationPoint(day), neptuneCommodity.countryBenchmarkKey, CountryBenchmarkData(data.toList))
    }.toList
  }

  private def logInvalid(msg: String, commodities: Map[NeptuneCommodity, _]) = log.warn(msg + commodities.keys.mkString(", "))
}