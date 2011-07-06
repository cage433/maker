package starling.curves

import starling.market.Index
import starling.daterange.Period
import starling.quantity.Quantity
import starling.market.SingleIndex
import starling.daterange._

case class SwapPrice(index : SingleIndex, averagingPeriod : DateRange) extends EnvironmentDifferentiable with PriceKey{

  def market = index.market
  private lazy val averagingDays = averagingPeriod.days.filter(index.isObservationDay)

  def curveKey = ForwardCurveKey(index.market)
  def calc_dP(env : Environment)  = index.market.standardShift

  def shiftedEnvs(env : Environment, dP : Quantity)  = {
    (
      env.shiftIndexForwardPrice(index, averagingPeriod, -dP),
      env.shiftIndexForwardPrice(index, averagingPeriod, dP)
    )
  }
  def periodKey = Some(DateRangePeriod(averagingPeriod))
  def quantityValue(env : Environment) = env.averagePrice(index, averagingPeriod)

  override def riskMarket = index.reportDisplayName

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = {
    averagingPeriod match {
      case day : Day => {
        SwapPrice(
          index,
          EnvironmentDifferentiable.containingDifferentiablePeriod(index.businessCalendar, marketDay, tenor, day)
        )
      }
      case _ => this
    }
  }
}

