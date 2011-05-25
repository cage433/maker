package starling.reports.pivot

import starling.daterange._
import starling.instrument._
import starling.utils._
import starling.curves._
import starling.pivot._
import collection.Seq
import collection.immutable.Set
import starling.gui.api._
import starling.market.FuturesMarket
import starling.instrument.SingleCalendarSpreadOption
import starling.market.FuturesFrontPeriodIndex
import starling.marketdata.{MarketDataKey, MarketData}

/**
 * Holds the general pivot report code.
 *
 * A pivot report has standard trade and portfolio fields plus fields defined by each PivotReportType
 *
 * Running a pivot report returns ReportData.
 *  ReportData has
 *     trade -> trade details
 *     trade -> utp
 *     portfolio -> portfolio details
 *     portfolio -> utp
 *   For each PivotReportType
 *     utp -> List[row data] and fields to extract field value from row data 
 *
 */


trait PivotReportType {
  type T

  def name: String

  def slidable: Boolean

  def run(context: ReportContext, utps: Map[UTPIdentifier, UTP]): PivotReport[_ <: PivotReportRow]

  def availablePages: List[String] = List()

  def fields: List[PivotReportField[T]] = List()

  def label : PivotReportTypeLabel = PivotReportTypeLabel(name, slidable, fields.map(_.name))
}

trait PivotReport[R <: PivotReportRow] {
  def rows(id : UTPIdentifier, instrument: UTP): List[R]

  def combine(rows: List[R], reportSpecificChoices : ReportSpecificChoices): List[R] = rows

  def scale(row: R, volume: Double): R

  def fields: List[PivotReportField[R]]

  import PivotReport._
  import ReportSpecificChoices._
  def reportSpecificOptions : ReportSpecificOptions = new ReportSpecificOptions(
    collapseOptions_str -> List(true, false),
    showEqFutures_str -> List(false, true),
    futuresAsSpreads_str -> List(false, true),
    futuresAsSwaps_str -> List(false, true),
    useSkew_str -> List(true, false),
    tenor_str -> List(MonthChoiceText, WeekChoiceText, DayChoiceText)
  )
}






/**
 * Reports which extend this get some combining implemented here.
 * TODO - move this to an objects static method, as all reports which extend it
 * also override combine and then call super.combine
 */
trait RiskFactorSplittingPivotReport[R <: RiskPivotReportRow[R] with PivotRowShareableByRiskFactor[R]] extends PivotReport[R]{
  def marketDay : DayAndTime
  override def combine(rows : List[R], reportSpecificChoices : ReportSpecificChoices) = {
    val collapseOptions = reportSpecificChoices.getOrElse(PivotReport.collapseOptions_str, true)
    var combinedRows = rows.flatMap{row => 
      row.splitByEnvironmentDifferentiable(marketDay, reportSpecificChoices)
    }
    combinedRows = combinedRows.map(_.setCollapseOptions(collapseOptions).asOriginalType)
    combinedRows = combinedRows.map(_.setPeriodForDisplay(reportSpecificChoices.getOrElse(PivotReport.tenor_str, Month)))
    combinedRows
  }                                                                                                                                                                                                                                              ;
}


object PivotReport{
  val collapseOptions_str = "Collapse Options"
  val showEqFutures_str = "Show Eq Futures"
  val futuresAsSpreads_str = "Futures as spreads"
  val futuresAsSwaps_str = "Futures as swaps"
  val useSkew_str = "Skew"
  val utp_str: String = "UTP"
  val usd_str: String = "USD"
  val riskType_str: String = "Risk Type"
  val riskCommodity_str: String = "Risk Commodity"
  val riskMarket_str: String = "Risk Market"
  val riskPeriod_str: String = "Risk Period"
  val riskVolume_str: String = "Risk Volume"
  val error_str: String = "Error"
  val tenor_str = "Tenor"
  val strategy_str = "Strategy"
  val atmVega_str = "ATM Vega"
  val lots_str: String = "Lots"
  val price_unit_str = "Price Unit"

  def spreadMonthsByStrategyAndMarket(utps : Map[UTPIdentifier, UTP]) = {
    var spreadMonths = Map[(Option[String], FuturesMarket), Set[Month]]()
    utps.foreach{
      case (id, SingleCalendarSpreadOption(mkt, _, mth1, mth2, _, _, _)) => {
        val key = (id.getAttribute(strategy_str), mkt)
        spreadMonths += (key ->  (Set(mth1, mth2) ++ spreadMonths.getOrElse(key, Set())))
      }
      case _ =>
    }
    spreadMonths
  }

  def swapIndicesByStrategy(utps : Map[UTPIdentifier, UTP]) = {
    var map = Map[Option[String], Set[FuturesFrontPeriodIndex]]()
    def addIndex(id : UTPIdentifier, idx : FuturesFrontPeriodIndex){
      val key = id.getAttribute(strategy_str)
      map += (key -> (map.getOrElse(key, Set[FuturesFrontPeriodIndex]()) + idx))
    }
    utps.foreach{
      case (id, SingleAsianOption(idx : FuturesFrontPeriodIndex, _, _, _, _)) => addIndex(id, idx)
      case (id, SingleCommoditySwap(idx : FuturesFrontPeriodIndex, _, _, _, _, _)) => addIndex(id, idx)
      case _ => 
    }
    map
  }

}
abstract class PivotReportField[R](val name: String) {
  def value(reportRow: R): Any
  def pivotFieldDetails = FieldDetails(name)
}

class NoConstructorArgsEnvironment extends Environment(null, null)


case class FooField[R <: PivotReportRow](field: PivotReportField[R]) {
  def value(row: (R, Seq[Any])) = {
    field.value(row._1)
  }

  def name = field.name

  def pivotFieldDetails: FieldDetails = field.pivotFieldDetails
}

case class SlideDetails(stepNumbers: List[Int])
object SlideDetails {
  val Null = new SlideDetails(List())
  def createFromSliders(sliders:EnvironmentSlider*):SlideDetails = new SlideDetails(sliders.map(_.stepNumber).toList)
}

class PivotReportData[R <: PivotReportRow](data: Map[UTPIdentifier, Either[List[R], Throwable]], val report: PivotReport[R],
  val slideDetails: SlideDetails, val availablePages: List[String]) {

  val fields = report.fields.map(new FooField(_))

  def defaultCombinedRows = {
    val allRows = data.flatMap {
      case (_, Left(rows)) => rows
      case _ => Nil
    }
    report.combine(allRows.toList, ReportSpecificChoices.create(report.reportSpecificOptions.default))
  }

  def mergedResult(utpIdMap: scala.collection.Map[UTPIdentifier, (Double, Seq[Any])], reportSpecificChoices:ReportSpecificChoices): List[(R, Seq[Any])] = {
    var resultRowsWithoutError: scala.collection.mutable.ListBuffer[R] = scala.collection.mutable.ListBuffer[R]()
    utpIdMap.foreach {
      case (id, (volume, _)) => data(id) match {
        case Left(rows:List[R]) => resultRowsWithoutError ++= rows.map(r => report.scale(r, volume))
        case Right(_) => 
      }
    }
    val combinedRowsWithoutErrors = report.combine(resultRowsWithoutError.toList, reportSpecificChoices).map{
      row =>
        val tradeFieldValues : Seq[Any] = utpIdMap.getOrElse(row.utpID, (0.0, Seq[Any]()))._2
        (row, tradeFieldValues)

    }
    combinedRowsWithoutErrors
  }


  def rows(instrumentID: UTPIdentifier, volume: Double) = {
    data(instrumentID) match {
      case Left(rows) => rows.map(r => Left(report.scale(r, volume))).toList
      case Right(_) => List()
    }
  }

  def errorIDs = data.flatMap(tuple => tuple._2 match {
    case Left(_) => List()
    case Right(t) => List(tuple._1)
  })

  def errors = Map() ++ data.flatMap(tuple => tuple._2 match {
    case Left(_) => List()
    case Right(t) => List(tuple._1 -> t.toString)
  })
}
object PivotReportData {
  def run[T <: PivotReportRow](report: PivotReport[T], instruments: Map[UTPIdentifier, UTP], slideDetails: SlideDetails,
    availablePages: List[String]): PivotReportData[T] = {

    val data = Map[UTPIdentifier, Either[List[T], Throwable]]() ++ (for ((instrumentid, utp) <- instruments) yield {
      instrumentid -> (try {
        utp.priceAndVolKeys(Day(2001, 1, 1).endOfDay)
        Left(report.rows(instrumentid, utp))
      } catch {
        case t: Throwable => t.printStackTrace; Right(t)
      })
    })

    new PivotReportData(data, report, slideDetails, availablePages) //.asInstanceOf[PivotReport[T]])
  }
}
case class TradeSetTables(tables: List[STable])
case class ReportData(reports: List[PivotReportData[_ <: PivotReportRow]], errorUTPs: Map[UTPIdentifier, UTP], recorded:Set[(ObservationPoint, MarketDataKey, MarketData)])
