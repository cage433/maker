package starling.curves.readers

import starling.db.{MarketDataEntry, MarketDataSource, DB}
import starling.utils.conversions.RichTraversables

//import starling.utils.ImplicitConversions._
import starling.daterange.{ObservationPoint, Day}
import starling.quantity.{UOM, Quantity}
import starling.marketdata._
import starling.richdb.RichDB
import starling.market.{Aluminium, Commodity}

class NeptuneBenchmarksMarketDataSource(neptuneDB:RichDB) extends MarketDataSource {
  def read(day: Day):Map[(Day, Day, MarketDataType), List[MarketDataEntry]] = {
    Map((day, day, GradeHubBenchmarkDataType) -> readGradeHubBenchmarks(day))
  }

  def readGradeHubBenchmarks(day:Day):List[MarketDataEntry] = {
    val sql = """
    select
         geographical_location.description as location_description
        ,material.description as material_description
        ,category.description as category_description
        ,benchmark_price

     from [Neptune].[live].[benchmarkprices] benchmarkprices
      left join [Neptune].[live].[material] material on material.code = benchmarkprices.material_code
      left join [Neptune].[live].[category] category on category.code = benchmarkprices.category_code
      left join [Neptune].[live].[geographical_location] geographical_location on geographical_location.code = benchmarkprices.location_code
    """
    val rows = neptuneDB.queryWithResult(sql) { rs => {
      val commodityName = rs.getString("material_description")
      val commodity = if (commodityName == "Primary Aluminium") Aluminium else Commodity.fromName(commodityName)
      (commodity,
       (Grade(rs.getString("category_description")), Hub(rs.getString("location_description")), rs.getDouble("benchmark_price"))
      )
    }}
    rows.groupBy(_._1).map { case (commodity, values) => {
      val data = values.toList.map { case (_, (grade, hub, price)) => {
        (grade,hub) -> Quantity(price, UOM.USD)
      }}
      MarketDataEntry( ObservationPoint(day), GradeHubBenchmarkMarketDataKey(commodity), GradeHubBenchmarkData(data.toList))
    } }.toList
  }
}