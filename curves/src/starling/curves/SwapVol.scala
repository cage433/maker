package starling.curves

import starling.quantity.Percentage
import starling.daterange._
import starling.quantity.Quantity
import starling.quantity.UOM
import starling.market.SingleIndex

case class SwapVol(index : SingleIndex, averagingPeriod : DateRange) extends VolKey with EnvironmentDifferentiable{
  private val averagingDays : List[Day] = index.observationDays(averagingPeriod)

  def calc_dP(env : Environment)  = Percentage(0.005)

  def quantityValue (env : Environment) : Quantity = {
    val daysInFuture = averagingDays.filter(_.endOfDay > env.marketDay)
    val vol = daysInFuture match {
      case Nil => Percentage(1e-6)
      case _ => {
        val strike = index.forwardPrice(env.instrumentLevelEnv, averagingDays.last, ignoreShiftsIfPermitted = false)
        env.swapVol(index, SimpleDateRange.containingRange(daysInFuture), strike)
      }
    }
    Quantity(vol.decimalValue, UOM.SCALAR)
  }

  val periodKey = Some(DateRangePeriod(averagingPeriod))

  val curveKey = OilAtmVolCurveKey(index.forwardPriceMarket)

  def shiftedEnvs(env : Environment, dP : Quantity) = {
    index.shiftedUnderlyingVols(env, averagingPeriod, dP)
  }
  override def riskMarket = index.reportDisplayName

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = {
    averagingPeriod match {
      case day : Day => {
        SwapVol(
          index,
          EnvironmentDifferentiable.containingDifferentiablePeriod(index.businessCalendar, marketDay, tenor, day)
        )
      }
      case _ => this
    }
  }
}

