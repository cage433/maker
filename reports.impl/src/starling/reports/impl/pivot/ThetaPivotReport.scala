package starling.reports.impl.pivot

import starling.quantity.UOM
import starling.daterange._
import starling.instrument._
import starling.pivot.{SumPivotQuantityFieldDetails, PivotQuantity}
import starling.curves._
import starling.reports.impl.pivot.PivotReport._
import starling.utils.cache.CacheFactory
import starling.concurrent.MP._
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}

class ThetaPivotReport(context: ReportContext, utps: Map[UTPIdentifier, UTP]) extends RiskFactorSplittingPivotReport[ThetaPivotReport.T] {

  val spreadMonthsByStrategyAndMarket = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  private val calcThetaCache = CacheFactory.getCache("ThetaPivotReport.calcTheta", unique = true)
  private def calcTheta(utp : UTP, changeOnlyTimeAndDiscounts : Boolean) : PivotQuantity = calcThetaCache.memoize(
    (utp, changeOnlyTimeAndDiscounts),
    (tuple : (UTP, Boolean)) => {
    PivotQuantity.calcOrCatch(utp.theta(context.environment, context.thetaDayAndTime, UOM.USD, changeOnlyTimeAndDiscounts))
  })
   
  def rows(utpID:UTPIdentifier, instrument: UTP) = {
    List(Theta(utpID, instrument, PivotQuantity.NULL))
  }

  override def combine(rows : List[Theta], reportSpecificChoices : ReportSpecificChoices) = {
    val showInLots = reportSpecificChoices.getOrElse(lots_str, false)
    val atmVega = reportSpecificChoices.getOrElse(atmVega_str, false)
    val changeOnlyTimeAndDiscounts = !atmVega
    // hack - if not using atmVega (i.e. using interpolated vols), we also change theta so that the
    // change in day doesn't affect interpolated vols. This sucks but is being done to emulate
    // JF's code. Whould be gone by end of 2011 Q1
    val combinedRows = new PivotUTPRestructurer(context.environment, reportSpecificChoices,
      spreadMonthsByStrategyAndMarket, swapIndices).transformUTPs(rows).mpMap{
      case UTPWithIdentifier(utpID, utp) => {
        val (unitUTP, volume) = utp.asUnitUTP
        Theta(utpID, utp, calcTheta(unitUTP, changeOnlyTimeAndDiscounts) * volume)
      }
    }.toList
    val convertedRows = if (showInLots) {
      LotConverter.convertThetas(combinedRows)
    } else {
      combinedRows
    }
    super.combine(convertedRows, reportSpecificChoices).filterNot(_.theta.isAlmostZero)
  }

  def scale(theta: Theta, volume: Double) = theta * volume

  def fields = ThetaPivotReport.fields

  def envForSplitting = context.environment

  override def reportSpecificOptions = super.reportSpecificOptions :+ atmVega :+ lots
}

case class Theta(
  utpID : UTPIdentifier,
  utp : UTP,
  theta: PivotQuantity,
  diff : Option[EnvironmentDifferentiable] = None,
  period : Option[Period] = None,
  collapseOptions : Boolean = true,
  scale : Double = 1.0
) 
  extends PivotRowWithEnvironmentDifferentiable[Theta] with PivotRowShareableByRiskFactor[Theta] 
{
  def *(volume: Double) = copy(scale = scale * volume)
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def setDiff(diff : EnvironmentDifferentiable) = copy(diff = Some(diff), period = diff.periodKey)
}

object ThetaRiskFields extends RiskPivotFields[Theta]


object ThetaPivotReport extends PivotReportType {
  type T = Theta
  def name = "Theta"

  def slidable = true

  def run(context: ReportContext, utps: Map[UTPIdentifier, UTP]) = {
    new ThetaPivotReport(context, utps)
  }

  override def fields = List(
    new PivotReportField[Theta]("Theta") {
      def value(reportRow: Theta) = reportRow.theta * reportRow.scale
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    }
  ) ::: ThetaRiskFields.riskFields
}
