package starling.reports.impl.pivot

import starling.daterange._
import starling.instrument._
import starling.utils._
import starling.curves._
import starling.pivot._
import collection.Seq
import starling.gui.api._
import starling.market.FuturesMarket
import starling.instrument.SingleCalendarSpreadOption
import starling.market.FuturesFrontPeriodIndex
import starling.marketdata.{MarketDataKey, MarketData}
import starling.marketdata.ReferenceDataLookup
import collection.immutable.{Map, Set}

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

  import ReportSpecificOptions._
  def reportSpecificOptions : ReportSpecificOptions = new ReportSpecificOptions(
    collapseOptions,
    showEqFutures,
    futuresAsSpreads,
    futuresAsSwaps,
    useSkew,
    oilTenors,
    metalsTenors,
    valuationCurrency
  )
  def zeroFields:Set[Field] = Set()
}






/**
 * Reports which extend this get some combining implemented here.
 * also override combine and then call super.combine
 */
// TODO [01 Dec 2010] move this to an objects static method, as all reports which extend it
trait RiskFactorSplittingPivotReport[R <: RiskPivotReportRow[R] with PivotRowShareableByRiskFactor[R]] extends PivotReport[R]{
  def envForSplitting : Environment
  override def combine(rows : List[R], reportSpecificChoices : ReportSpecificChoices) = {
    val collapseOptions = reportSpecificChoices.getOrElse(ReportSpecificOptions.collapseOptionsLabel, true)
    var combinedRows = rows.flatMap{row => 
      row.splitByEnvironmentDifferentiable(envForSplitting, reportSpecificChoices)
    }
    combinedRows = combinedRows.map(_.setCollapseOptions(collapseOptions).asOriginalType)
    combinedRows = combinedRows.map(_.setPeriodForDisplay(reportSpecificChoices.getOrElse(ReportSpecificOptions.tenorLabel, Month)))
    combinedRows
  }
}


object PivotReport{
  import ReportSpecificOptions._ 
  def validReportSpecificChoices(desk : Option[Desk]) = desk match {
    case Some(Desk.Titan) => List(valuationCurrency, metalsTenors)
    case Some(Desk(_, _, Some(_ : EAIDeskInfo))) => List(collapseOptions, showEqFutures, futuresAsSpreads, futuresAsSwaps, useSkew, oilTenors, atmVega, positionType, priceUnit, lots)
    case None => Nil
    case _ => throw new Exception("Unexpected desk " + desk)
  }

  def spreadMonthsByStrategyAndMarket(utps : Map[UTPIdentifier, UTP]) = {
    var spreadMonths = Map[(Option[String], FuturesMarket), Set[Month]]()
    utps.foreach{
      case (id, SingleCalendarSpreadOption(mkt, _, mth1, mth2, _, _, _)) => {
        val key = (id.getAttribute(strategyLabel), mkt)
        spreadMonths += (key ->  (Set(mth1, mth2) ++ spreadMonths.getOrElse(key, Set())))
      }
      case _ =>
    }
    spreadMonths
  }

  def swapIndicesByStrategy(utps : Map[UTPIdentifier, UTP]) = {
    var map = Map[Option[String], Set[FuturesFrontPeriodIndex]]()
    def addIndex(id : UTPIdentifier, idx : FuturesFrontPeriodIndex){
      val key = id.getAttribute(strategyLabel)
      map += (key -> (map.getOrElse(key, Set[FuturesFrontPeriodIndex]()) + idx))
    }
    utps.foreach{
      case (id, SingleAsianOption(idx : FuturesFrontPeriodIndex, _, _, _, _)) => addIndex(id, idx)
      case (id, SinglePeriodSwap(idx : FuturesFrontPeriodIndex, _, _, _, _, _, _, _, _)) => addIndex(id, idx)
      case _ => 
    }
    map
  }

}
abstract class PivotReportField[R](val name: String) {
  def value(reportRow: R): Any
  def pivotFieldDetails = FieldDetails(name)
}

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
        case Left(rows:List[_]) => resultRowsWithoutError ++= rows.map(r => report.scale(r, volume))
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

  def errors: Map[UTPIdentifier, StackTrace] = Map() ++ data.flatMap(tuple => tuple._2 match {
    case Left(_) => List()
    case Right(t) => List( (tuple._1, StackTrace(t)) )
  })
}
object PivotReportData {
  def run[T <: PivotReportRow](report: PivotReport[T], instruments: Map[UTPIdentifier, UTP], slideDetails: SlideDetails,
    availablePages: List[String], referenceDataLookup : ReferenceDataLookup): PivotReportData[T] = {

    val data = Map[UTPIdentifier, Either[List[T], Throwable]]() ++ (for ((instrumentid, utp) <- instruments) yield {
      instrumentid -> (try {
        utp.priceAndVolKeys(Environment(NullAtomicEnvironment(Day(2001, 1, 1).endOfDay, referenceDataLookup)))
        Left(report.rows(instrumentid, utp))
      } catch {
        case t: Throwable => {
//          Log.error("Throwable caught from valuing UTP: " + utp, t)
          Right(t)
        }
      })
    })

    new PivotReportData(data, report, slideDetails, availablePages) //.asInstanceOf[PivotReport[T]])
  }
}
case class TradeSetTables(tables: List[STable])
case class ReportData(reports: List[PivotReportData[_ <: PivotReportRow]], errorUTPs: Map[UTPIdentifier, UTP], recorded:Set[(ObservationPoint, MarketDataKey, MarketData)])
