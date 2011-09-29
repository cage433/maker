package starling.reports.impl.pivot

import starling.instrument.UTP
import starling.curves.{Environment, EnvironmentDifferentiable}
import starling.daterange.{DayAndTime, Period}
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.quantity.UOM
import starling.tradestore.TradeAndFields
import starling.reports.impl.pivot.PivotReport._
import starling.pivot.{PivotQuantityFieldDetails, SumPivotQuantityFieldDetails, PivotQuantity}


case class CurrentPriceChangeRow(
  utpID : UTPIdentifier,
  utp : UTP,
  value: PivotQuantity,
  diff : Option[EnvironmentDifferentiable] = None,
  period : Option[Period] = None,
  collapseOptions : Boolean = true,
  scale : Double = 1.0
)
  extends PivotRowShareableByRiskFactor[CurrentPriceChangeRow] with PivotRowWithEnvironmentDifferentiable[CurrentPriceChangeRow]
{
  def *(volume: Double) = copy(scale = scale * volume)
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def setDiff(diff : EnvironmentDifferentiable) = copy(diff = Some(diff), period = diff.periodKey)
}

class CurrentPriceChangeReport(d1:Environment, d2:Environment, utps : Map[UTPIdentifier, UTP]) extends RiskFactorSplittingPivotReport[CurrentPriceChangeRow] {

  val spreadMonthsByStrategyAndMarket = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  def fields = List(
    new PivotReportField[CurrentPriceChangeRow]("'Current Price' Change") {
      def value(reportRow: CurrentPriceChangeRow) = reportRow.value
      override def pivotFieldDetails = new PivotQuantityFieldDetails(name)
    }
    ) ::: CurrentPriceChangeRowFields.riskFields

  def scale(row: CurrentPriceChangeRow, volume: Double) = row * volume

  def rows(utpID : UTPIdentifier, utp:UTP) = {
    List(CurrentPriceChangeRow(utpID, utp, PivotQuantity.NULL))
  }

  override def combine(rows : List[CurrentPriceChangeRow], reportSpecificChoices : ReportSpecificChoices) = {

    import starling.concurrent.MP._

    val combinedRows = new PivotUTPRestructurer(d1, reportSpecificChoices, spreadMonthsByStrategyAndMarket, swapIndices).transformUTPs(rows).mpFlatMap{
      case UTPWithIdentifier(utpID, utp) => {
        val (unitUTP, volume) = utp.asUnitUTP

        val d1Price = PivotQuantity.calcOrCatch(unitUTP.price(d1))
        val d2Price = PivotQuantity.calcOrCatch(unitUTP.price(d2))

        val change = d2Price - d1Price
        List(
          CurrentPriceChangeRow(utpID, utp, change)
        ).filterNot(_.value.isAlmostZero)
      }
    }
    super.combine(combinedRows, reportSpecificChoices)
  }
  def marketDay = d1.marketDay
  override def reportSpecificOptions = super.reportSpecificOptions :+
      (atmVega_str -> List(false, true))
}

object CurrentPriceChangeRowFields extends RiskPivotFields[CurrentPriceChangeRow]
