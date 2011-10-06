package starling.curves.readers

import starling.db.{MarketDataEntry, MarketDataSource}
import starling.dbx.QueryBuilder._
import starling.daterange.{ObservationPoint, Day}
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import collection.immutable.Map
import starling.marketdata._
import starling.utils.{ImplicitConversions, Log}
import starling.quantity.Quantity


class NeptuneGradeAreaBenchmarkUtil(neptuneDB:RichDB) extends Log {
  def read(day: Day) = {
    val today = Day.today
    val entries = {
      val (rows, invalidRows) = neptuneDB.queryWithResult(
          select("material.description as material_description", "category_code", "location_code", "benchmark_price")
            from("[live].[benchmarkprices] benchmarkprices")
        leftJoin("[live].[material] material", "material.code" eql "benchmarkprices.material_code")) { rs =>

        NeptuneCommodity(rs.getString("material_description")) →
          (GradeCode(rs.getString("category_code")), AreaCode(rs.getString("location_code")), rs.getDouble("benchmark_price"))
      }.toMultiMap.partitionKeys(_.isValid)

      invalidRows.ifDefined { logInvalid("Cannot import grade-area benchmarks for neptune commodities: ", _) }

      rows.map { case (neptuneCommodity, values) =>
        val data = values.map { case (grade, area, price) => (grade, area) → (day → neptuneCommodity.toQuantity(price)) }

        val nestedData: ImplicitConversions.NestedMap[(GradeCode, AreaCode), Day, Quantity] = data.toNestedMap
        MarketDataEntry(ObservationPoint(today), neptuneCommodity.gradeAreaBenchmarkKey, GradeAreaBenchmarkData(nestedData))
      }.toList
    }

    Map((today, today, GradeAreaBenchmarkDataType) → entries)
  }

  private def logInvalid(msg: String, commodities: Map[NeptuneCommodity, _]) = log.warn(msg + commodities.keys.mkString(", "))
}