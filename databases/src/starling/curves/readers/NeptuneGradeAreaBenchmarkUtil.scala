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
import starling.quantity.UOM
import starling.market.FuturesExchange
import starling.market.NeptuneCommodity
import starling.market.NeptunePricingExchange
import starling.market.Commodity
import scala.collection.mutable.{Set => MSet}

object NeptunePriceUOM{
  def apply(commodity : NeptuneCommodity, exchange : NeptunePricingExchange) : UOM = {
    val futuresMarket = exchange.marketFor(commodity) match {
      case Some(futuresMarket) => futuresMarket
      case None => Commodity.standardFuturesMarket(commodity)
    }
    futuresMarket.priceUOM
  }
}


class NeptuneGradeAreaBenchmarkUtil(neptuneDB:RichDB) extends Log {
  def read(day: Day) = {
    val today = Day.today
    val badCommodityNames = MSet[String]()
    val badAreaCodes = MSet[String]()
    var rows = List[(NeptuneCommodity, (GradeCode, AreaCode, Quantity))]()
    neptuneDB.query(
          select("material.description as material_description", "category_code", "location_code", "benchmark_price")
            from("[live].[benchmarkprices] benchmarkprices")
        leftJoin("[live].[material] material", "material.code" eql "benchmarkprices.material_code")) { rs =>

      val neptuneCommodityName = rs.getString("material_description")
      val commodityOption = Commodity.neptuneCommodityFromNeptuneName(neptuneCommodityName) 
      val gradeCode = GradeCode(rs.getString("category_code"))
      val areaCode = AreaCode(rs.getString("location_code"))
      val rate = rs.getDouble("benchmark_price")

      (commodityOption, NeptunePricingExchange.fromArea(areaCode)) match {
        case (None, _) => badCommodityNames += neptuneCommodityName
        case (_, None) => badAreaCodes += areaCode.code
        case (Some(commodity), Some(exchange)) => {
          val price = Quantity(rate, NeptunePriceUOM(commodity, exchange))
          rows = (commodity → (gradeCode, areaCode, price)) :: rows
        }
      }
    }

    if (! badCommodityNames.isEmpty)
      log.warn("Unable to import grade area benchmarks for unrecognised commodities " + badCommodityNames.mkString(", "))

    if (! badAreaCodes.isEmpty)
      log.warn("Unable to import grade area benchmarks for areas " + badAreaCodes.mkString(", "))

    val entries = {
      rows.toMultiMap.map { case (neptuneCommodity, values) =>
        val data = values.map { case (grade, area, price) => (grade, area) → (day → price) }

        val nestedData: ImplicitConversions.NestedMap[(GradeCode, AreaCode), Day, Quantity] = data.toNestedMap
        MarketDataEntry(ObservationPoint(today), neptuneCommodity.gradeAreaBenchmarkKey, GradeAreaBenchmarkData(nestedData))
      }.toList
    }

    Map((today, today, GradeAreaBenchmarkDataType) → entries)
  }

  private def logInvalid(msg: String, commodities: Map[NeptuneCommodity, _]) = log.warn(msg + commodities.keys.mkString(", "))
}
