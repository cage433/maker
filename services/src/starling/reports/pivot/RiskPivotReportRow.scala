package starling.reports.pivot

import starling.quantity.{UOM, Quantity}
import starling.quantity.UOM._
import starling.pivot.model._
import starling.varcalculator._
import starling.daterange._
import starling.daterange.TimeOfDay._
import starling.instrument._
import starling.pivot.{Field, SumPivotQuantityFieldDetails, FieldDetails, PivotQuantity}
import starling.utils.{Stopwatch, Log}
import starling.curves._
import starling.reports.pivot.PivotReport._
import starling.quantity.Quantity._
import starling.market.rules.CommonPricingRule
import starling.market._
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.marketdata.{PeriodComparator, PeriodFieldDetails}

trait PivotReportRow{
  def utpID : UTPIdentifier

}

/**
 * Trait is implemented by the row types of each
 * pivot report so that numbers can be broken out
 * by risk market and period. Sometimes (e.g. theta)
 * by equally division across all risk factors
 */
trait RiskPivotReportRow[T] extends PivotReportRow{
  self : T =>
  def asStandardType : T = self
  def *(x : Double) : T
  def utpID : UTPIdentifier
  def utp : UTP
  def scale : Double
  def marketName : String
  def riskType : Option[String]
  def riskCommodity : Option[String]
  def period : Option[Period]
  def collapseOptions : Boolean
  def setPeriod(period : Option[Period]) : RiskPivotReportRow[T]
  def setPeriodForDisplay(tenor : TenorType) : T = {
    /* For P&L Explanation and risk to work sensibly we need to operate on periods that exclude days
       before the current market day - e.g. balance of month. However for display purposes we just want
       to show the whole period
    */
    def containingPeriod(d : Day) = tenor match {
      case `Month` => d.containingMonth
      case `Week` => d.week
      case`Day` => d
    }
    (period match {
      case Some(DateRangePeriod(dateRange)) => {
        val p1 = containingPeriod(dateRange.firstDay)
        val p2 = containingPeriod(dateRange.lastDay)
        if (p1 == p2)
          setPeriod(Some(DateRangePeriod(p1)))
        else
          this
      }
      case _ => this
    }).asStandardType
  }
}

/**
 * Report rows that extend this trait are split automatically by the associated EnvironmentDifferentiables. These are
 * determined by the reportSpecificChoices, and cash UTPs are replaced with an appropriate UTP, depending on which
 * trade type they were originally associated with.
 */
trait PivotRowShareableByRiskFactor[T]{
  self : RiskPivotReportRow[T] =>
  def *(x : Double) : T

  def setDiff(diff : EnvironmentDifferentiable) : PivotRowShareableByRiskFactor[T]

  def splitByEnvironmentDifferentiable(marketDay : DayAndTime, reportSpecificChoices : ReportSpecificChoices) : List[T] = {
    val utp : UTP = self.utp match {
      case BankAccount(_, Some(mkt : FuturesMarket), None, period : DateRangePeriod) => Future(mkt, period.period, 0.0(mkt.priceUOM), 1.0(mkt.uom))
      case BankAccount(_, Some(mkt : FuturesMarket), None, SpreadPeriod(front : Month, back : Month)) => 
          FuturesCalendarSpread(mkt, front, back, 0.0(mkt.priceUOM), 0.0(mkt.priceUOM), 1.0(mkt.uom))
      case BankAccount(_, None, Some(index : FuturesFrontPeriodIndex), period : DateRangePeriod) => SingleCommoditySwap(index, 0.0(index.priceUOM), 1.0(index.uom), period.period, cleared = true)
      case BankAccount(_, None, Some(multiIndex : MultiIndex), period : DateRangePeriod) => {
        val index = multiIndex.arbitraryIndexToAssignCashTo
        SingleCommoditySwap(index, 0.0(index.priceUOM), 1.0(index.uom), period.period, cleared = true)
      }
      case BankAccount(_, None, Some(index : SingleIndex), period : DateRangePeriod) => {
        SingleCommoditySwap(index, 0.0(index.priceUOM), 1.0(index.uom), period.period, cleared = true)
      }

      case CashInstrument(_, _, _, Some(Left(index : Index)), Some(DateRangePeriod(period))) => {
        // we default to a cleared swap with a common pricing rule. this may not be correct.
        SingleCommoditySwap(index, 0.0(index.priceUOM), 1.0(index.uom), period, cleared = true, pricingRule = CommonPricingRule)
      }
      case CashInstrument(_, _, _, Some(Right(mkt: FuturesMarket)), Some(period : Period)) => {
        period match {
          case DateRangePeriod(drp) => Future(mkt, drp, 0.0(mkt.priceUOM), 1.0(mkt.uom))
          case SpreadPeriod(front:Month,back:Month) => FuturesCalendarSpread(mkt, front, back, 0.0(mkt.priceUOM), 0.0(mkt.priceUOM), 1.0(mkt.uom))
        }
      }
      case _ => self.utp
    }
    splitByEnvironmentDifferentiable(utp, marketDay, reportSpecificChoices)
  }

  private def splitByEnvironmentDifferentiable(utp :UTP, marketDay : DayAndTime, reportSpecificChoices : ReportSpecificChoices) : List[T] = {
    val (priceDiffs, volDiffs) = PivotReportUtils.priceAndVolKeys(utp, marketDay, reportSpecificChoices)
    val diffs = (priceDiffs ++ volDiffs).toList
    if (diffs.isEmpty)
      utp match {
        case _ : CashInstrument => List(self.asStandardType)
        case _ => List(self.setPeriod(utp.periodKey).asStandardType) // Don't think this can happen
      }
    else {
      diffs.map {
        diff => 
          setDiff(diff).setCollapseOptions(collapseOptions) * (1.0 / diffs.size)
      }
    }
  }
  def asOriginalType : T = self.asStandardType
  def setCollapseOptions(co : Boolean) : PivotRowShareableByRiskFactor[T]
}

/**
 * Rows which extend this trait implement some of their methods from
 * the associated EnvironmentDifferentiable option
 */
trait PivotRowWithEnvironmentDifferentiable[T] extends RiskPivotReportRow[T]{
  self : T =>
  def diff : Option[EnvironmentDifferentiable]
  def riskType = diff.map(_.riskType)
  def riskCommodity = diff.map(_.riskCommodity)
  def marketName = diff match {
    case Some(d) => d.riskMarket
    case None => ""
  }
}

/**
 * Generic fields across all reports
 */
trait RiskPivotFields[T <: RiskPivotReportRow[T]]{
  def riskFields = List(                                            
    new PivotReportField[T]("Risk Type") {
      def value(reportRow : T) = reportRow.riskType match {
        case Some(x) => x
        case None => "N/A"
      }
    },
    new PivotReportField[T]("Risk Commodity") {
      def value(reportRow : T) = reportRow.riskCommodity match {
        case Some(x) => x
        case None => "N/A"
      }
    },

    new PivotReportField[T](riskMarket_str) {
      def value(reportRow: T) = {
        val market = reportRow.marketName 
        if (market != "" && !reportRow.collapseOptions && reportRow.utp.riskMarketExtra != "")
          market + " " + reportRow.utp.riskMarketExtra
        else
          market
      }
    },
    new PivotReportField[T]("Risk Period") {
      def value(reportRow : T) = reportRow.period.getOrElse("NOPERIOD")
      override def pivotFieldDetails = new PeriodFieldDetails(name) {
        override def nullValue() = "NOPERIOD"
      }
    },

    new PivotReportField[T]("UTP Type") {
      def value(reportRow : T) = reportRow.utp.instrumentType.toString
    }
    )
}

object OptionalPeriodLabelComparator extends Ordering[Any] {
  def compare(x: Any, y: Any) = {
    ((x,y): @unchecked) match {
      case (OptionalPeriodLabel(Some(p1)), OptionalPeriodLabel(Some(p2))) => {
        PeriodComparator.compare(p1,p2)
      }
    }
  }
}
