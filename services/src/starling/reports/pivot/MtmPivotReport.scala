package starling.reports.pivot

import java.lang.Throwable
import starling.curves.{ReportContext, Environment}
import starling.quantity.{UOM, Quantity}
import starling.utils.Stopwatch
import starling.curves.EnvironmentDifferentiable
import starling.daterange.Period
import starling.reports.pivot.PivotReport._
import starling.utils.cache.CacheFactory
import starling.curves.AtomicEnvironment
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.instrument._
import starling.pivot._

/**
 * Shows the mtm and undiscounted mtm for an instrument
 */

object MtmPivotReport extends PivotReportType {
  type T = Mtm
  def name = "P&L"
  def slidable = true
  def run(context: ReportContext, utps: Map[UTPIdentifier, UTP]) = {
    new MtmPivotReport(context.environment, utps)
  }

  val pnlFieldName = "P&L"
  val pnlNoCostsFieldName = "P&L (No Costs)"
  val costsFieldsName = "Costs"

  override def fields = List(
    new PivotReportField[Mtm]("Known/Estimate") {
      def value(row: Mtm) = row.asset match {
        case Left(asset) => if (asset.known) "Known" else "Estimated"
        case _ => "E"
      }
    },
    new PivotReportField[Mtm]("Past/Future") {
      def value(row: Mtm) = row.asset match {
        case Left(asset) => if (asset.isPast) "Past" else "Future"
        case _ => "E"
      }
    },
    new PivotReportField[Mtm]("Asset Delivery Day") {
      def value(row: Mtm) =  row.asset match {
        case Left(asset) => NullableDay(asset.settlementDay)
        case _ => NullableDay.Null
      }
      override def pivotFieldDetails = new NullableDayFieldDetails(name)
    },
    new PivotReportField[Mtm]("Settlement Market") {
      def value(row: Mtm) = row.asset match {
        case Left(asset) => asset.market.toString
        case _ => "E"
      }
    },
    new PivotReportField[Mtm](pnlFieldName) {
      def value(row: Mtm) = row.asset match {
        case Left(asset) => PivotQuantity(asset.mtm * row.scale)
        case Right(e) => new PivotQuantity(e)
      }
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    },
    new PivotReportField[Mtm](pnlNoCostsFieldName) {
      def value(row: Mtm) = row.assetNoCosts match {
        case Some(Left(asset)) => PivotQuantity(asset.mtm * row.scale)
        case Some(Right(e)) => new PivotQuantity(e)
        case _ => PivotQuantity.NULL
      }
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    },
    new PivotReportField[Mtm](costsFieldsName) {
      def value(row: Mtm) = row.costs match {
        case Some(Left(asset)) => PivotQuantity(asset.mtm * row.scale)
        case Some(Right(e)) => new PivotQuantity(e)
        case _ => PivotQuantity.NULL
      }
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    }
  ) ::: MtmRiskFields.riskFields
}

case class Mtm(
  utpID : UTPIdentifier,
  utp : UTP,
  asset : Either[Asset, Throwable],
  assetNoCosts : Option[Either[Asset, Throwable]] = None,
  costs : Option[Either[Asset, Throwable]] = None,
  diff : Option[EnvironmentDifferentiable] = None,
  period : Option[Period] = None,
  collapseOptions : Boolean = true,
  scale : Double = 1.0
) 
  extends PivotRowWithEnvironmentDifferentiable[Mtm] with PivotRowShareableByRiskFactor[Mtm] 
{
  def *(x : Double) = copy(scale = scale * x)
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def setDiff(diff : EnvironmentDifferentiable) = copy(diff = Some(diff), period = diff.periodKey)
  override def toString = {
    "utpID " + utpID + "\n" +
    "utp   " + utp + "\n" +
    "asset " + asset + "\n" +
    "assetNoCosts " + assetNoCosts + "\n" +
    "diff  " + diff + "\n" +
    "period " + period + "\n" +
    "collapseOptions " + collapseOptions + "\n" +
    "scale " + scale + "\n"
  }

}

object MtmRiskFields extends RiskPivotFields[Mtm]

class MtmPivotReport(@transient environment:Environment, @transient utps : Map[UTPIdentifier, UTP]) extends RiskFactorSplittingPivotReport[Mtm] with Serializable {
  def fields = MtmPivotReport.fields

  def scale(row: Mtm, volume: Double) = row.copy(scale = row.scale * volume)

  def rows(utpID : UTPIdentifier, instrument: UTP) = {
    List(Mtm(utpID, instrument, null))
  }

  val spreadMonthsByStrategyAndMarket = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  private val calcAssetsCache = CacheFactory.getCache("MtmPivotReport.calcAssets", unique = true)
  private def calcAssets(utp : UTP) : List[Either[Asset, Throwable]] = calcAssetsCache.memoize(
    (utp),
    (tuple : (UTP)) => {
      try {
        utp.assets(environment).assets.map{asset => Left(asset.copyMtm(asset.mtm * environment.spotFXRate(UOM.USD, asset.mtm.uom)))}
      } catch {
        case e => List(Right(e))
      }
    }
  )

  override def combine(rows : List[Mtm], reportSpecificChoices : ReportSpecificChoices) : List[Mtm] = {
    import starling.concurrent.MP._

    val combinedRows = new PivotUTPRestructurer(environment, reportSpecificChoices, spreadMonthsByStrategyAndMarket, swapIndices).transformUTPs(rows).mpFlatMap{
      case UTPWithIdentifier(utpID, utp) => {
        val (unitUTP, volume) = utp.asUnitUTP

        calcAssets(unitUTP).map{
          asset=> {
            utp match {
              case ci: CashInstrument if CashInstrumentType.isCost(ci.cashInstrumentType) => {
                Mtm(utpID, utp, asset, None, Some(asset)) * volume
              }
              case _ => Mtm(utpID, utp, asset, Some(asset)) * volume
            }
          }
        }
      }
    }
    super.combine(combinedRows, reportSpecificChoices)
  }
  def marketDay = environment.marketDay
  override def zeroFields = Set(Field(MtmPivotReport.pnlFieldName))
}
