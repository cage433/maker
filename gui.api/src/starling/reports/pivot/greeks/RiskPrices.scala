package starling.reports.pivot.greeks

import starling.pivot.PivotQuantity
import starling.quantity.{Percentage, Quantity}


object RiskPrices {
  val Null = RiskPrices(Map())
}

case class RiskPrices(prices:Map[(Any,Any),PivotQuantity]) {
  def this(market:Any, period:Any, price:PivotQuantity) = this(Map( (market,period) -> price))
  def ++ (other:RiskPrices) = RiskPrices(this.prices ++ other.prices)
  def averagePrice = if (prices.isEmpty) None else Some(prices.values.sum / prices.values.size)
}

object VolatilityMeasure{
  def apply(q : Quantity) : VolatilityMeasure = {
    if (q.uom.isScalar)
      VolatilityMeasure(Right(Percentage(q.value)))
    else
      VolatilityMeasure(Left(q.value))
  }
}

case class VolatilityMeasure(measure:Either[Double,Percentage])

object RiskVols {
  val Null = RiskVols(Map())
}

case class RiskVols(vols:Map[(Any,Any),VolatilityMeasure]) {
  def this(market:Any, period:Any, vol:VolatilityMeasure) = this(Map( (market,period) -> vol))
  def ++ (other:RiskVols) = RiskVols(this.vols ++ other.vols)
}