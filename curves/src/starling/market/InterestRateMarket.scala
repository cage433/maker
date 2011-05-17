package starling.market

import collection.immutable.Map
import starling.varcalculator.{ForwardRateRiskFactor, RiskFactor}
import starling.quantity.{Percentage, Quantity, UOM}
import starling.curves.{ForwardForwardDiscountCurve, DiscountCurveKey}
import starling.daterange.{DateRange, SimpleDateRange, DayAndTime}

/*
  Represents a currency's market for interest rate derivatives
 */
case class InterestRateMarket(ccy : UOM)
  extends Market with HasImpliedVol
{
  def priceUOM = UOM.SCALAR
  val name = ccy.toString
  val uomName = name

  def standardShift = Quantity(1e-4, UOM.SCALAR)

  def positionUOM = ccy

  def curveKey = DiscountCurveKey(ccy)
}

object InterestRateMarket{
  def allMarkets = UOM.currencies.map(InterestRateMarket(_))
}
