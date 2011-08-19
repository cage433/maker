package starling.services.trinity

import java.text.DecimalFormat

import starling.daterange._
import starling.db.DB
import starling.gui.api.CurveLabel
import starling.pivot.controller.{PivotTableConverter, PivotGrid}
import starling.utils.ImplicitConversions._
import com.trafigura.services.trinity.CommodityRate
import starling.market._
import starling.calendar.{BusinessCalendar, BusinessCalendars}
import starling.pivot.{SomeSelection, Field, PivotFieldsState}
import java.util.TreeMap
import starling.curves.{CurveViewerFields, CurveViewer}
import starling.pivot.model.{AxisCell, PivotTableModel}

case class TrinityCommodityCurveKey(commodity:String, exchange:String, currency:String, contract:String)

class FCLGenerator(businessCalendars:BusinessCalendars, curveViewer: CurveViewer) {
  notNull("curveViewer" → curveViewer)

  private val pivotFieldsState = PivotFieldsState(dataFields = List(Field("Price")),
    rowFields = List(Field("Market"), Field("Period")),
    filters = Field("Market") -> SomeSelection(Set("Panamax T/C Average (Baltic)")) :: Nil,
    reportSpecificChoices = scala.collection.immutable.TreeMap.empty[String,Any] + (CurveViewerFields.showInterpolatedReportOption -> true)

  )

  def generate(curveLabel: CurveLabel): Map[TrinityCommodityCurveKey,Traversable[CommodityRate]] = {
    val pivotTable = PivotTableModel.createPivotTableData(curveViewer.curve(curveLabel), Some(pivotFieldsState))
    FCLGenerator.extractData(businessCalendars.US_UK, PivotTableConverter(table = pivotTable).createGrid())
  }
}

object FCLGenerator {
  def extractData(ukUS:BusinessCalendar, data: PivotGrid) = data.rowData.zip(data.mainData).toList.partialMap {
    case (Array(AxisCell.ValueText(Index.PublishedIndex(index)), periodCell), Array(priceCell)) => {
      val trinityKey = TrinityUploadCodeMapper.trinityCurveKeys(index)
      val price = priceCell.doubleValue.get
      val month = periodCell.value.value.value.asInstanceOf[DateRange]
      val trinityDay = month.lastDay.addBusinessDays(ukUS, 2)
      trinityKey → CommodityRate(trinityDay.toString("dd-MMM-yyyy"), price, price, trinityDay.toString("dd/MM/yyyy"),
                                 trinityKey.exchange, trinityKey.contract, "1", "TONNE")
    }
  }.groupInto(_._1, _._2)
}

object TrinityUploadCodeMapper {
//  select distinct currency.description, currency.name, MRATE_CURVE.TAG, MRATE_CURVE.CURRENCYAGAINST
//  from currency
//  join MRATE_CURVE on MRATE_CURVE.CURRENCY = CURRENCY.NAME
//  where basetag = 'BALTIC'

//  Handymax T/C Average	BHM	BALTIC	USD
//  Supramax T/C Average	BSM	BALTIC	USD
//  Capsize	BCM	BALTIC	USD
//  BC7 T/C Average	BC7	BALTIC	USD
//  Panamax T/C Average	BPM	BALTIC	USD
//
  val trinityCurveKeys = Map(
    "Baltic Supramax T/C Avg" -> TrinityCommodityCurveKey("BSM", "BALTIC", "USD", "BALTIC SUPRAMAX Future"),
    "Panamax T/C Average (Baltic)" -> TrinityCommodityCurveKey("BPM", "BALTIC", "USD", ""),
    "Capesize T/C Average (Baltic)" -> TrinityCommodityCurveKey("BCM", "BALTIC", "USD", "")
  ).mapKeys(Index.publishedIndexFromName)
}