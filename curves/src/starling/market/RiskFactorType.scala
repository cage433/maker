package starling.market

import starling.quantity.Quantity._
import starling.curves._
import starling.marketdata.MarketData
import starling.quantity.{Percentage, Quantity, UOM}
import starling.daterange.{Month, Day, DateRange, DayAndTime}
import starling.varcalculator._
import collection.SortedMap
import collection.immutable.{TreeMap, Map}

/**
 * Represents markets and types of risk to which we are exposed. Typical examples are
 * WTI prices, LME Copper vols, EUR/USD spot FX rate, GBP Interest rates.
 *
 */
trait RiskFactorType {

  /**
   * The name of this type of risk factor (Price, SpotFX, Forward Rates)
   */
  def typeName:String

  /**
   * The standard units positions wrt this risk factor are reported
   */
  def positionUOM : UOM

  /**
   * The shift size used in numerical differentiation wrt risk factors associated with this market.
   */
  def standardShift : Quantity


  /**
   * A string that represents the underlying for this risk factor. E.g. a ForwardPriceRiskFactorType(ICE WTI) would
   * have an underlying of "ICE WTI"
   */
  def underlying: String

  /**
   * A string that represents the parent underlying for this risk factor. E.g. a ForwardPriceRiskFactorType(ICE WTI) would
   * have an underlying of "WTI"
   */
  def higherUnderlying: String = ""

}

object RiskFactorType{
  val priceString = "Price"
  val volString = "Vol"
  val equityString = "Equity"
  val spotFXString = "Spot FX"
  val forwardRateString = "Forward Rate"
}
/**
 * Risk factors used in VaR, e.g. price, spotfx, interest rate
 */
trait VaRRiskFactorType extends RiskFactorType {
  /** The VaR scenario generator creates arrays of shifts to risk factors - these are then grouped by market
    * and the next two methods are then used to build new perturbed environments
    */
  def curveKey : CurveKey
  def buildCurveObject(marketDayAndTime : DayAndTime, prices : Map[VaRRiskFactor, Double]) : CurveObject

}

case class ForwardPriceRiskFactorType(market: CommodityMarket) extends VaRRiskFactorType {

  def typeName = RiskFactorType.priceString
  def standardShift = market.standardShift

  def positionUOM = market.uom

  def buildCurveObject(marketDayAndTime: DayAndTime, riskFactorPrices: Map[VaRRiskFactor, Double]) = {
    var prices = Map.empty[DateRange, Double]
    riskFactorPrices.foreach {
      case (rf: ForwardPriceRiskFactor, price) => {
        val period = rf.period(marketDayAndTime)
        market.tenor match {
          case Day => {
            prices += period.firstDay -> price
            prices += period.lastDay -> price
          }
          case Month => {
            prices += period -> price
          }
        }
      }
    }
    ForwardCurve.create(market, marketDayAndTime, prices)
  }

  def curveKey = market.curveKey

  def underlying = market.toString

  override def higherUnderlying = market.commodity.toString
}

case class VolatilityRiskFactorType(market: HasImpliedVol) extends RiskFactorType {
  def typeName = RiskFactorType.volString

  def standardShift = new Quantity(0.01)

  def positionUOM = market.positionUOM

  def underlying = market.toString
}


case class EquitiesRiskFactorType(ric:RIC) extends VaRRiskFactorType {
  def underlying = ric.code
  def standardShift = Quantity(0.1, ric.priceUOM)
  def positionUOM = UOM.SHARE
  def typeName = RiskFactorType.equityString
  def buildCurveObject(marketDayAndTime: DayAndTime, prices: Map[VaRRiskFactor, Double]) = {
    assert(prices.size == 1)
    prices.get(EquityRiskFactor(ric)) match {
      case Some(price) => new EquityPriceCurveObject(marketDayAndTime, ric, Quantity(price, ric.priceUOM))
      case _ => throw new IllegalStateException("Invalid risk factors ")
    }
  }
  def curveKey = EquityPriceCurveKey(ric)
}

/**
 *  Represents an atomic Spot FX market - USD/something. We can report deltas wrt this market, we can't for cross currencies
 * as they aren't atomic
 */
case class SpotUSDFXRiskFactorType(ccy : UOM) extends VaRRiskFactorType{
  def typeName = RiskFactorType.spotFXString

  private val priceUOM = UOM.USD / ccy

  def curveKey = USDFXRateCurveKey(ccy)
  def standardShift = Quantity(0.01, priceUOM)
  def positionUOM = ccy
  override val toString = "USD/" + ccy


  def buildCurveObject(marketDayAndTime: DayAndTime, prices: Map[VaRRiskFactor, Double]) = {
    assert(prices.size == 1)
    prices.get(SpotFXRiskFactor(ccy)) match {
      case Some(rate) => USDFXRate(marketDayAndTime, rate (priceUOM))
      case _ => throw new IllegalStateException("Invalid risk factors ")
    }
  }

  def underlying = ccy.toString
}

case class ForwardRateRiskFactorType(market: InterestRateMarket) extends VaRRiskFactorType {
  def typeName = RiskFactorType.forwardRateString

  def standardShift = market.standardShift

  def positionUOM = market.ccy

  def buildCurveObject(marketDayAndTime: DayAndTime, prices: Map[VaRRiskFactor, Double]) = {
    val marketDay = marketDayAndTime
    val rates = Map.empty[DateRange, Percentage] ++ prices.map {
      case (rf, rate) => (rf.period(marketDay) -> Percentage(rate))
    }
    new ForwardForwardDiscountCurve(market.ccy, marketDayAndTime, rates)
  }


  def underlying = market.toString

  def curveKey = market.curveKey
}
