package starling.services.trinity

import java.text.DecimalFormat

import starling.curves.CurveViewer
import starling.daterange._
import starling.db.DB
import starling.gui.api.CurveLabel
import starling.pivot.{Field, PivotFieldsState}
import starling.pivot.controller.{PivotTableConverter, PivotGrid}
import starling.pivot.model.PivotTableModel

import starling.utils.ImplicitConversions._
import starling.market.{TrinityMarket, FuturesMarket, Market}


class FCLGenerator(uploadMapper: TrinityUploadCodeMapper, curveViewer: CurveViewer) {
  notNull(uploadMapper, curveViewer)

  def generate(curveLabel: CurveLabel) = {
    val pivotFieldsState = PivotFieldsState(
      dataFields = List(Field("Price")),
      rowFields = List(Field("Market"), Field("Period"))
    )

    val pivotTable = PivotTableModel.createPivotTableData(curveViewer.curve(curveLabel), Some(pivotFieldsState))

    FCLGenerator.generateLines(uploadMapper.uploadCode _, PivotTableConverter(table = pivotTable).createGrid())
  }
}

object FCLGenerator {
  def generateLines(codeLookup:FuturesMarket=>String, data: PivotGrid): List[String] = {
    val format = new DecimalFormat("0000000.0000")

    val rows = data.rowData.zip(data.mainData)

    rows.toList.flatMapO { case (Array(marketName, periodCell), Array(priceCell)) => {
      try {
        val market = Market.futuresMarketFromName(marketName.valueText)
        val trinityCode = codeLookup(market)
        val price = format.format(priceCell.doubleValue.get)
        val period = periodCell.value.value.value.asInstanceOf[DateRange].firstDay

        println(">> " + market + " " + trinityCode + " " + period)

        Some("3300C%s     %sFF%s0000000000000000000000000000000CN" % (trinityCode, period.toString("YYMMdd"), price))
      } catch {
        case e: Exception => None
      }
    } }
  }
}

class TrinityUploadCodeMapper(trinityDB:DB) {
  lazy private val lookup = (trinityDB.queryWithResult("select * from T_Commcode") { rs => {
    (rs.getString("COMMODITY"), rs.getString("EXCHANGE"), rs.getString("CURRENCY")) -> rs.getString("CODE")
  }}).toMap

  def uploadCodeOption(market:FuturesMarket) = {
    TrinityMarket.marketToTrinityCode.get(market) match {
      case None => None
      case Some(trinityCommodityCode) => {
        val key = (trinityCommodityCode, market.exchange.name, market.currency.toString)
        lookup.get(key)
      }
    }
  }
  def uploadCode(market:FuturesMarket) = uploadCodeOption(market).getOrElse(throw new Exception("No upload code for " + market))
}