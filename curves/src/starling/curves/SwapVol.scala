package starling.curves

import starling.quantity.Percentage
import starling.daterange._
import starling.quantity.Quantity
import starling.quantity.UOM
import starling.market.SimpleSingleIndex

case class SwapVol(index : SimpleSingleIndex, averagingPeriod : DateRange) extends VolKey with EnvironmentDifferentiable{
  private val averagingDays : List[Day] = index.observationDays(averagingPeriod)

  def volMarket = index.market


  def calc_dP(env : Environment)  = Percentage(0.005)

  def quantityValue (env : Environment) : Quantity = {
    val daysInFuture = averagingDays.filter(_.endOfDay > env.marketDay)
    val vol = daysInFuture match {
      case Nil => Percentage(1e-6)
      case _ => {
        val strike = env.forwardPrice(index.market, index.observedPeriod(daysInFuture.last))
        env.swapVol(index, SimpleDateRange.containingRange(daysInFuture), strike)
      }
    }
    Quantity(vol.decimalValue, UOM.SCALAR)
  }

  val periodKey = Some(DateRangePeriod(averagingPeriod))

  val curveKey = OilAtmVolCurveKey(index.market)

  def shiftedEnvs(env : Environment, dP : Quantity) = {
    val firstObservedDay = averagingDays.map(index.observedPeriod(_).firstDay).sortWith(_<_).head
    val lastObservedDay = averagingDays.map(index.observedPeriod(_).lastDay).sortWith(_>_).head
    val observedPeriod = DateRange(firstObservedDay, lastObservedDay)
    val dV = Percentage(dP.checkedValue(UOM.SCALAR))
    val upEnv = env.shiftVol(index.market, Some(averagingPeriod), observedPeriod, dV)
    val downEnv = env.shiftVol(index.market, Some(averagingPeriod), observedPeriod, -dV)
    (downEnv, upEnv)
  }
  override def riskMarket = index.reportDisplayName

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = {
    averagingPeriod match {
      case day : Day => {
        SwapVol(
          index,
          EnvironmentDifferentiable.containingDifferentiablePeriod(index.market.businessCalendar, marketDay, tenor, day)
        )
      }
      case _ => this
    }
  }
}

