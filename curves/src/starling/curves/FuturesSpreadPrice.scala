package starling.curves

import starling.quantity.Quantity
import starling.daterange._
import starling.market.{FuturesSpreadMarket, FuturesMarket}

case class FuturesSpreadPrice(market : FuturesMarket, period: Period) extends EnvironmentDifferentiable with PriceKey{
  
  private val (firstKey, secondKey) = (market, period) match {
      case (fsm: FuturesSpreadMarket, DateRangePeriod(month: Month)) => {
        (PriceDifferentiable(fsm.market1, month), PriceDifferentiable(fsm.market2, month))
      }
      case (f: FuturesMarket, SpreadPeriod(firstMonth: Month, secondMonth: Month)) => {
        (PriceDifferentiable(market, firstMonth), PriceDifferentiable(market, secondMonth))
      }
    }

  def curveKey = ForwardCurveKey(market)

  def calc_dP(env : Environment) : Quantity = {
    Quantity(0.1, market.priceUOM)
  }
  def shiftedEnvs(env : Environment, dP : Quantity) : (Environment, Environment) = {
    val (dnFront, upFront) = firstKey.shiftedEnvs(env, dP / 2.0)
    val (dnFrontDnBack, dnFrontUpBack) = secondKey.shiftedEnvs(dnFront, dP / 2.0)
    val (upFrontDnBack, upFrontUpBack) = secondKey.shiftedEnvs(upFront, dP / 2.0)
    (dnFrontUpBack, upFrontDnBack)
  }
  def periodKey = Some(period)
  def quantityValue (env : Environment) : Quantity = firstKey.quantityValue(env) - secondKey.quantityValue(env)

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = this
}
