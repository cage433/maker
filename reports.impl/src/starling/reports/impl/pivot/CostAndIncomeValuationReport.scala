package starling.reports.impl.pivot

import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.instrument.UTP
import starling.curves.Environment
import starling.instrument.physical.PhysicalMetalAssignmentOrUnassignedSalesQuota
import starling.quantity.Quantity
import starling.curves.ReportContext
import starling.pivot.PivotQuantity
import starling.pivot.SumPivotQuantityFieldDetails
import starling.instrument.physical.PhysicalMetalAssignment
import starling.daterange.Period
import starling.curves.EnvironmentDifferentiable
import starling.titan.valuation.{CostsAndIncomeUnallocatedAssignmentValuation, CostsAndIncomeAllocatedSaleAssignmentValuation, CostsAndIncomeAllocatedPurchaseAssignmentValuation}

case class CostsAndIncomeValuationRow(
  utpID : UTPIdentifier, 
  utp : UTP,

  value : Quantity,
  premium : Quantity, 
  fixedQuantity : Quantity,
  weightGainOrLoss : Quantity,
  freightParity : Quantity,

  scale : Double = 1.0,
  period : Option[Period] = None, 
  diff : Option[EnvironmentDifferentiable] = None,
  collapseOptions : Boolean = true,
  error : Option[Throwable] = None
)

  extends PivotRowWithEnvironmentDifferentiable[CostsAndIncomeValuationRow] with PivotRowShareableByRiskFactor[CostsAndIncomeValuationRow]{
  def isError = error.isDefined
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setDiff(diff : EnvironmentDifferentiable) = copy(diff = Some(diff), period = diff.periodKey)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def *(x : Double) = copy(scale = scale * x)
}


object CostsAndIncomeValuationRow{
  def apply(utpID : UTPIdentifier, utp : UTP, error : Throwable)  : CostsAndIncomeValuationRow = {
    CostsAndIncomeValuationRow(utpID, utp, null, null, null, null, null, error = Some(error))
  }
}


class CostAndIncomeValuationReport(@transient env:Environment, @transient utps : Map[UTPIdentifier, UTP]) extends RiskFactorSplittingPivotReport[CostsAndIncomeValuationRow] {

  def marketDay = env.marketDay

  def rows(id : UTPIdentifier, utp: UTP): List[CostsAndIncomeValuationRow] = {
    try {
      utp match {
        case ass : PhysicalMetalAssignment => {
          val valuation = ass.costsAndIncomeValuation(env)
          List(
            valuation.valuationDetails match {
              case Right(value) => {
                valuation match {
                  case ass : CostsAndIncomeAllocatedPurchaseAssignmentValuation => {
                    CostsAndIncomeValuationRow(
                      id,
                      utp,
                      value.value,
                      value.premium,
                      value.fixedQuantity,
                      Quantity.NULL,
                      Quantity.NULL
                    )
                  }
                  case ass : CostsAndIncomeAllocatedSaleAssignmentValuation => {
                    CostsAndIncomeValuationRow(
                      id,
                      utp,
                      value.value,
                      value.premium,
                      value.fixedQuantity,
                      ass.weightGainOrLoss,
                      Quantity.NULL
                    )
                  }
                  case ass : CostsAndIncomeUnallocatedAssignmentValuation => {
                    CostsAndIncomeValuationRow(
                      id,
                      utp,
                      value.value,
                      value.premium,
                      value.fixedQuantity,
                      ass.weightGainOrLoss,
                      ass.freightParity
                    )
                  }
                }
              }
              case Left(error) => throw new Exception(error)
            }
          )
        }
        case _ => Nil
      }
    }
    catch {
      case e =>  
        Nil
    }
  }

//  def combine(rows: List[CostsAndIncomeValuationRow], reportSpecificChoices : ReportSpecificChoices): List[CostsAndIncomeValuationRow] = rows

  def scale(row: CostsAndIncomeValuationRow, volume: Double): CostsAndIncomeValuationRow = row

  def fields: List[PivotReportField[CostsAndIncomeValuationRow]] = CostAndIncomeValuationReport.fields

}

object CostAndIncomeValuationReport extends PivotReportType{
  type T = CostsAndIncomeValuationRow
  val slidable = true

  val name = "C&I"

  def run(context: ReportContext, utps: Map[UTPIdentifier, UTP]) = {
    new CostAndIncomeValuationReport(context.environment, utps)
  }
  override def fields = List(
    new PivotReportField[T]("C&I Value") {
      def value(row: T) = if (row.isError) new PivotQuantity(row.error.get) else PivotQuantity(row.value)
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    },
    new PivotReportField[T]("C&I Premium") {
      def value(row: T) = if (row.isError) new PivotQuantity(row.error.get) else PivotQuantity(row.premium)
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    },
    new PivotReportField[T]("C&I Fixed Quantity") {
      def value(row: T) = if (row.isError) new PivotQuantity(row.error.get) else PivotQuantity(row.fixedQuantity)
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    },
    new PivotReportField[T]("C&I Weight Gain Or Loss") {
      def value(row: T) = if (row.isError) new PivotQuantity(row.error.get) else PivotQuantity(row.fixedQuantity)
      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    }
  )
}

