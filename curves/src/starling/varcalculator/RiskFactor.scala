package starling.varcalculator

import starling.quantity.UOM._
import scala.None
import starling.daterange._
import starling.curves._
import interestrate.DayCountActualActual
import starling.quantity.{Percentage, Quantity, UOM}
import starling.market._
import collection.immutable.Map
import starling.models.DefaultRiskParameters
import math._

/**Represents something to which we are exposed to risk for VaR purposes
 */
trait RiskFactor {

  def market : AnyRef
  
  def riskFactorType: RiskFactorType

  def shiftedEnvironments(env: Environment): ShiftedEnvironments

  /**
   * Shift environment up and down `times` number of times.
   *
   * The default implementation just chains a number of shifts but this doesn't work for everything
   * and should be overridden if that's the case.
   */
  def shiftedEnvironments(env: Environment, times: Int): ShiftedEnvironments = {
    val envs = shiftedEnvironments(env)
    ShiftedEnvironments(
      env, shiftDown(envs.downEnv, times - 1),
      shiftUp(envs.upEnv, times - 1),
      envs.dP
    )
  }

  protected def shiftUp(env: Environment, remaining: Int): Environment = remaining match {
    case 0 => env
    case _ => shiftUp(shiftedEnvironments(env).upEnv, remaining - 1)
  }

  protected def shiftDown(env: Environment, remaining: Int): Environment = remaining match {
    case 0 => env
    case _ => shiftDown(shiftedEnvironments(env).downEnv, remaining - 1)
  }
}

trait VaRRiskFactor extends RiskFactor {

  /**
   * This is the price observed historically, used to calculate historic vols and
   * correlations for VaR. Due to the high correlation observed in metals/oil markets
   * it is normally sufficient to use the price for the last period of the risk factor,
   * the average could be used instead but this is unlikely to have much affect on VaR.
   *
   * The price returned should also be adjusted for London close times. So, for example,
   * if a price for Shanghai Copper is requested the price will be the
   * Shanghai Copper price adjusted to what it would most likely be at London close.
   *
   * This method should only be used for VaR (for the reasons above).
   */
  def representativePriceForVaR(env: Environment): Quantity

  /**
   * This is the actual price for this risk factor. It should not be adjusted or fudged in any way.
   */
  def price(env: Environment): Quantity

  override def riskFactorType: VaRRiskFactorType

  def nAhead: Option[NAhead]

  def period(marketDay: DayAndTime): DateRange

  /**
   * Merges 2 risk factors together. Used to optimise calculation of
   * parallel shift position for VaR reports. This matters for Asian options in
   * particular.
   */
  def merge(rf: VaRRiskFactor): VaRRiskFactor

  /**
   * For delta pivot report - risk instrument field
   */
  def riskInstrumentType : String
}

case class ForwardPriceRiskFactor(market: CommodityMarket, nPeriodsToStart: Int, nPeriodsToEnd: Int) extends VaRRiskFactor {
  assert(nPeriodsToStart >= 0, "Price risk factors must begin in the future " + this)
  assert(nPeriodsToStart <= nPeriodsToEnd, "Mis-ordered days to start and end " + this)

  def nAhead = Some(NAhead(nPeriodsToEnd, market.tenor.toString))

  def merge(rf: VaRRiskFactor) = rf match {
    case ForwardPriceRiskFactor(`market`, rhsPeriodsToStart, rhsPeriodsToEnd) =>
      ForwardPriceRiskFactor(market, nPeriodsToStart min rhsPeriodsToStart, nPeriodsToEnd max rhsPeriodsToEnd)
  }

  val riskFactorType = ForwardPriceRiskFactorType(market)

  def period(marketDay: DayAndTime) = {
    val p = DateRange(
      market.nthPeriod(marketDay, nPeriodsToStart).firstDay,
      market.nthPeriod(marketDay, nPeriodsToEnd).lastDay
      )
    p
  }

  def representativePriceForVaR(env: Environment) = env.forwardPrice(market, market.nthPeriod(env.marketDay, nPeriodsToEnd))

//  def price(env: Environment) = env.forwardPrice(market, market.nthPeriod(env.marketDay, nPeriodsToEnd))
  def price(env : Environment) : Quantity = {
    val (prices, discounts) = (nPeriodsToStart to nPeriodsToEnd).toList.map{n =>
      val period = market.nthPeriod(env.marketDay, n)
      val price = env.forwardPrice(market, period)
      val discount = env.discount(market.currency, period.lastDay)
      (price, discount)
    }.unzip
    prices.zip(discounts).map{case (p, d) => p * d}.sum / discounts.sum
  }

  def calc_dP(env: Environment): Quantity = {
    env.calc_dP(market, market.nthPeriod(env.marketDay, nPeriodsToEnd))
  }

  def shiftedEnvironments(env: Environment) = {
    val marketDay= env.marketDay
    val dP: Quantity = calc_dP(env)
    val upEnv = env.shiftPrice(market, period(marketDay), dP).copy(environmentParameters = DefaultRiskParameters)
    val downEnv = env.shiftPrice(market, period(marketDay), -dP).copy(environmentParameters = DefaultRiskParameters)
    ShiftedEnvironments(env, downEnv, upEnv, dP)
  }

  override def shiftedEnvironments(env: Environment, times: Int) = {
    val marketDay = env.marketDay
    val dP: Quantity = calc_dP(env)
    val upEnv = env.shiftPrice(market, period(marketDay), dP * times).copy(environmentParameters = DefaultRiskParameters)
    val downEnv = env.shiftPrice(market, period(marketDay), -dP * times).copy(environmentParameters = DefaultRiskParameters)
    ShiftedEnvironments(env, downEnv, upEnv, dP)
  }

  def riskInstrumentType = market match {
    case _ : FuturesMarket => "Future"
    case _ : SwapMarket => "Swap"
    case _ => "Unkonwn instrument for " + market
  }
}

case class VolatilityRiskFactor(market: HasImpliedVol, period: DateRange) extends RiskFactor {
  val riskFactorType = VolatilityRiskFactorType(market)

  val dP = Percentage(0.0050)

  def price(env: Environment) = throw new UnsupportedOperationException
  def vol(env: Environment) = env.atmImpliedVol(market, period)

  def shiftedEnvironments(env: Environment) = {
    val upEnv = env.shiftVol(market, None, period, dP).copy(environmentParameters = DefaultRiskParameters)
    val downEnv = env.shiftVol(market, None, period, -dP).copy(environmentParameters = DefaultRiskParameters)
    ShiftedEnvironments(env, downEnv, upEnv, dP)
  }
}

case class EquityRiskFactor(ric:RIC) extends VaRRiskFactor {
  def market = ric

  def shiftedEnvironments(env: Environment) = {
    val dP = riskFactorType.standardShift
    val upEnv = env.shiftEquityPrice(ric, dP)
    val downEnv = env.shiftEquityPrice(ric, -dP)
    ShiftedEnvironments(env, downEnv, upEnv, dP)
  }
  def riskFactorType = EquitiesRiskFactorType(ric)
  def merge(rf: VaRRiskFactor) = rf
  def period(marketDay: DayAndTime) = marketDay.day
  def nAhead = None
  def price(env: Environment) = env.equityPrice(ric)
  def representativePriceForVaR(env: Environment) = env.equityPrice(ric)

  def riskInstrumentType = "Equity"
}

case class SpotFXRiskFactor(ccy: UOM) extends VaRRiskFactor {
  def market = ccy
  def nAhead = None

  def riskFactorType = SpotUSDFXRiskFactorType(ccy)

  def period(marketDay: DayAndTime) = marketDay.day

  def representativePriceForVaR(env: Environment) = env.spotFXRate(USD, ccy)
  def price(env: Environment) = env.spotFXRate(USD, ccy)

  def shiftedEnvironments(env: Environment) = {
    val dP = riskFactorType.standardShift
    val upEnv = env.shiftSpotFX(ccy, dP)
    val downEnv = env.shiftSpotFX(ccy, -dP)
    ShiftedEnvironments(env, downEnv, upEnv, dP)
  }

  def merge(rf: VaRRiskFactor) = rf match {
    case SpotFXRiskFactor(`ccy`) => this
  }

  def positionUOM = ccy


  def riskInstrumentType = "Currency"
}

case class ForwardRateRiskFactor(market: InterestRateMarket, nDaysToStart: Int, nDaysToEnd: Int) extends VaRRiskFactor {
  def nAhead = Some(NAhead(nDaysToEnd, Day.toString))

  val ccy = market.ccy

  def riskFactorType = ForwardRateRiskFactorType(market)

  def period(marketDay: DayAndTime): DateRange = SimpleDateRange(marketDay.day + nDaysToStart, marketDay.day + nDaysToEnd).tryToNormalise

  def representativePriceForVaR(env: Environment) = {
    new Quantity(
      env.forwardRate(
        ccy,
        period(env.marketDay),
        DayCountActualActual
        ).value
      )
  }

  def price(env: Environment) = representativePriceForVaR(env)

  def curveKey = DiscountCurveKey(ccy)

  def shiftedEnvironments(env: Environment) = {
    ShiftedEnvironments(
      env,
      env.shiftFwdFwdRate(ccy, period(env.marketDay), riskFactorType.standardShift),
      env.shiftFwdFwdRate(ccy, period(env.marketDay), riskFactorType.standardShift * -1),
      riskFactorType.standardShift
    )
  }

  def merge(rf: VaRRiskFactor) = rf match {
    case ForwardRateRiskFactor(riskFactorMarket, rhsDaysToStart, rhsDaysToEnd) =>
      ForwardRateRiskFactor(riskFactorMarket, nDaysToStart min rhsDaysToStart, nDaysToEnd max rhsDaysToEnd)
  }

  def riskInstrumentType = "FRA"
}

object ForwardRateRiskFactor {
  def apply(ccy: UOM, nDaysToStart: Int, nDaysToEnd: Int): ForwardRateRiskFactor =
    ForwardRateRiskFactor(InterestRateMarket(ccy), nDaysToStart, nDaysToEnd)

  def adjustment(marketDay: Day, period: DateRange, day: Day, dR: Quantity): Double = {
    if (day <= period.firstDay)
      1.0
    else {
      val time = (period.lastDay min day) daysSinceInYears period.firstDay
      math.exp(-dR.value * time)
    }
  }

}




