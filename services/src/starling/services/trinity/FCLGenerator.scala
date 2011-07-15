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

  private val pivotFieldsState = PivotFieldsState(dataFields = List(Field("Price")),
    rowFields = List(Field("Market"), Field("Period")))

  def generate(curveLabel: CurveLabel) = getData(curveLabel).map(_.toLine)

  def getData(curveLabel: CurveLabel): List[FCLRow] = {
    val pivotTable = PivotTableModel.createPivotTableData(curveViewer.curve(curveLabel), Some(pivotFieldsState))

    FCLGenerator.extractData(PivotTableConverter(table = pivotTable).createGrid(), uploadMapper.uploadCode _)
  }
}

case class FCLRow(trinityCode: String, price: Double, period: Day) {
  def toLine = "3300C%s     %sFF%s0000000000000000000000000000000CN" %
    (trinityCode, period.toString("YYMMdd"), FCLGenerator.priceFormat.format(price))
}

object FCLGenerator {
  val priceFormat = new DecimalFormat("0000000.0000")
  def generateLines(codeLookup: String => String, data: PivotGrid) = extractData(data, codeLookup).map(_.toLine)

  def extractData(data: PivotGrid, codeLookup: (String) => String) = data.rowData.zip(data.mainData).toList.flatMapO {
    case (Array(marketName, periodCell), Array(priceCell)) => try {
      val trinityCode = codeLookup(marketName.valueText)
      val price = priceCell.doubleValue.get
      val period = periodCell.value.value.value.asInstanceOf[DateRange].firstDay

      println(">> " + marketName.valueText + " " + trinityCode + " " + period)

      Some(FCLRow(trinityCode, price, period))
    } catch {
      case e: Exception => None
    }
  }
}

class TrinityUploadCodeMapper(trinityDB:DB) {
  lazy private val lookup = (trinityDB.queryWithResult("select * from T_Commcode") { rs => {
    (rs.getString("COMMODITY"), rs.getString("EXCHANGE"), rs.getString("CURRENCY")) → rs.getString("CODE")
  }}).toMap

  def uploadCode(marketName: String): String = uploadCodeOption(Market.futuresMarketFromName(marketName))
    .getOrElse(throw new Exception("No upload code for " + marketName))

  def uploadCodeOption(market:FuturesMarket) = TrinityMarket.marketToTrinityCode.get(market).flatMap { trinityCommodityCode =>
    val key = (trinityCommodityCode, market.exchange.name, market.currency.toString)
    lookup.get(key)
  }
}