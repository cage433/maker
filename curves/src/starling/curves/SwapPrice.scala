package starling.curves

import starling.market.Index
import starling.daterange.Period
import starling.quantity.Quantity
import starling.market.SingleIndex
import starling.daterange._

case class SwapPrice(index : SingleIndex, averagingPeriod : DateRange) extends EnvironmentDifferentiable with PriceKey{

  val market = index.market
  private lazy val averagingDays = averagingPeriod.days.filter(index.isObservationDay)
  private lazy val observedPeriods = averagingDays.map(index.observedPeriod(_)).distinct.toList

  def curveKey = ForwardCurveKey(market)
  def calc_dP(env : Environment)  = PriceDifferentiable(market, observedPeriods.last).calc_dP(env)

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
          EnvironmentDifferentiable.containingDifferentiablePeriod(index.market.businessCalendar, marketDay, tenor, day)
        )
      }
      case _ => this
    }
  }
}

