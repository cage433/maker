package starling.curves

import starling.market.FuturesMarket
import starling.daterange.Month
import starling.quantity.Quantity
import starling.daterange.SpreadPeriod
import starling.daterange.{TenorType, DayAndTime}

case class FuturesSpreadPrice(market : FuturesMarket, month1 : Month, month2 : Month) extends EnvironmentDifferentiable with PriceKey{
  
  private val frontKey = PriceDifferentiable(market, month1)
  private val backKey = PriceDifferentiable(market, month2)

  def curveKey = ForwardCurveKey(market)

  def calc_dP(env : Environment) : Quantity = {
    Quantity(0.1, market.priceUOM)
  }
  def shiftedEnvs(env : Environment, dP : Quantity) : (Environment, Environment) = {
    val (dnFront, upFront) = frontKey.shiftedEnvs(env, dP / 2.0)
    val (dnFrontDnBack, dnFrontUpBack) = backKey.shiftedEnvs(dnFront, dP / 2.0)
    val (upFrontDnBack, upFrontUpBack) = backKey.shiftedEnvs(upFront, dP / 2.0)
    (dnFrontUpBack, upFrontDnBack)
  }
  def periodKey = Some(SpreadPeriod(month1, month2))
  def quantityValue (env : Environment) : Quantity = frontKey.quantityValue(env) - backKey.quantityValue(env)

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = this
}
