package starling.services

import starling.utils.ImplicitConversions._
import java.text.DecimalFormat
import starling.utils.TrinityUpload
import starling.pivot.model.PivotTableModel
import starling.pivot.controller.{PivotTableConverter, PivotTable, PivotGrid}
import starling.gui.api.CurveTypeLabel._
import starling.gui.api.EnvironmentSpecificationLabel._
import starling.curves.{ClosesEnvironmentRule, CurveViewer}
import starling.marketdata.{PriceFixingsHistoryData, MarketData, PriceFixingsHistoryDataKey}
import starling.daterange._
import starling.market.{Level, FuturesMarket, Market}
import starling.pivot.{MarketValue, ColumnStructure, Field, PivotFieldsState}
import starling.db.{NormalMarketDataReader, MarketDataStore, MarketDataReader, DB}
import starling.gui.api.{PricingGroup, MarketDataSelection, MarketDataIdentifier, CurveLabel}

class TrinityUploadCodeMapper(trinityDB:DB) {

  lazy private val lookup = (trinityDB.queryWithResult("select * from T_Commcode") { rs => {
    (rs.getString("COMMODITY"), rs.getString("EXCHANGE"), rs.getString("CURRENCY")) -> rs.getString("CODE")
  }}).toMap

  def uploadCodeOption(market:FuturesMarket) = {
    Market.marketToTrinityCode.get(market) match {
      case None => None
      case Some(trinityCommodityCode) => {
        val key = (trinityCommodityCode, market.exchange.name, market.currency.toString)
        lookup.get(key)
      }
    }
  }
  def uploadCode(market:FuturesMarket) = uploadCodeOption(market).getOrElse(throw new Exception("No upload code for " + market))
}

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

    rows.map { case (Array(marketName, periodCell), Array(priceCell)) => {
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
    } }.toList.somes
  }
}

class XRTGenerator(marketDataStore: MarketDataStore) {
  private val key = PriceFixingsHistoryDataKey("EUR", Some("LIBOR"))
  private val periods = List(Tenor(Month, 1), Tenor(Month, 2), Tenor(Month, 3)).map(tenor => (Level.Close, StoredFixingPeriod.tenor(tenor)))

  def marketDataReader(): MarketDataReader = {
    new NormalMarketDataReader(marketDataStore, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.LimOnly))))
  }

  def generate(observationDay: Day) = {
    val data = marketDataReader.read(observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose), key).asInstanceOf[PriceFixingsHistoryData]
    val x: Map[(Level, StoredFixingPeriod), MarketValue] = data.fixings.toMap.filterKeys(periods)
    val t: Map[Tenor, MarketValue] = x.mapKeys { case (_, StoredFixingPeriod.Tenor(tenor)) => tenor }

    t.map { case (tenor, marketValue) => {
      "3301EUR08121900001.2862004.786971000.993398CN"
    }}

    List("3301EUR08121900001.2862004.786971000.993398CN")
  }
}