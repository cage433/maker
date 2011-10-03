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


class NeptuneGradeAreaBenchmarksMarketDataSource(neptuneDB:RichDB) extends MarketDataSource with Log {
  def read(day: Day) = Map((day, day, GradeAreaBenchmarkDataType) → readGradeAreaBenchmarks(day))

  def readGradeAreaBenchmarks(day:Day): List[MarketDataEntry] = {
    val (rows, invalidRows) = neptuneDB.queryWithResult(
        select("material.description as material_description", "category_code", "location_code", "benchmark_price")
          from("[live].[benchmarkprices] benchmarkprices")
      leftJoin("[live].[material] material", "material.code" eql "benchmarkprices.material_code")) { rs =>

      NeptuneCommodity(rs.getString("material_description")) →
        (GradeCode(rs.getString("category_code")), AreaCode(rs.getString("location_code")), rs.getDouble("benchmark_price"))
    }.toMultiMap.partitionKeys(_.isValid)

    invalidRows.ifDefined { logInvalid("Cannot import grade-area benchmarks for neptune commodities: ", _) }

    rows.map { case (neptuneCommodity, values) =>
      val data = values.map { case (grade, area, price) => (grade, area) → neptuneCommodity.toQuantity(price) }

      MarketDataEntry(ObservationPoint(day), neptuneCommodity.gradeAreaBenchmarkKey, GradeAreaBenchmarkData(data.toMap))
    }.toList
  }

  private def logInvalid(msg: String, commodities: Map[NeptuneCommodity, _]) = log.warn(msg + commodities.keys.mkString(", "))
}