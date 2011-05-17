package starling.services

import starling.pivot.controller.PivotGrid
import starling.market.Market
import starling.utils.ImplicitConversions._
import starling.daterange.DateRange
import java.text.DecimalFormat


object Trinity {
  def exportFuturesPrices(data: PivotGrid): Boolean = exportFuturesPrices(generateFuturesPricesExportFile(data))

  def exportFuturesPrices(lines: List[String]): Boolean = {
    lines.foreach(println)
    false
  }

  def generateFuturesPricesExportFile(data: PivotGrid) = {
    val format = new DecimalFormat("0000000.0000")

    val rows = data.rowData.zip(data.mainData)

    rows.map { case (Array(marketName, periodCell), Array(priceCell)) => {
      try {
        val trinityCode = Market.marketToTrinityCode(Market.fromName(marketName.value.value.value.toString))
        val price = format.format(priceCell.doubleValue.get)
        val period = periodCell.value.value.value.asInstanceOf[DateRange].firstDay

        Some("3300C%s     %sFF%s0000000000000000000000000000000CN" % (trinityCode, period.toString("YYMMdd"), price))
      } catch {
        case e: Exception => None
      }
    } }.toList.somes
  }
}