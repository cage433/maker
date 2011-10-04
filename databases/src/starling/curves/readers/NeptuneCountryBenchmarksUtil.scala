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


class NeptuneCountryBenchmarksUtil(neptuneDB: RichDB) extends Log {
  def read(day: Day) = {
    val today = Day.today
    val entries = {
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

        MarketDataEntry(ObservationPoint(today), neptuneCommodity.countryBenchmarkKey, CountryBenchmarkData(data.toMap))
      }.toList
    }
    Map((today, today, CountryBenchmarkDataType) → entries)
  }

  private def logInvalid(msg: String, commodities: Map[NeptuneCommodity, _]) = log.warn(msg + commodities.keys.mkString(", "))
}