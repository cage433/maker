package starling.instrument

import starling.daterange.TimeOfDay._
import starling.richdb.RichInstrumentResultSetRow
import cern.jet.random.Normal
import starling.utils.ImplicitConversions._
import starling.models._
import starling.quantity.{UOM, Quantity}
import starling.quantity.Quantity._
import starling.curves.Environment
import starling.daterange._
import java.lang.String
import starling.utils.Log
import starling.curves.FuturesSpreadPrice
import starling.daterange.SpreadPeriod
import starling.curves.EnvironmentDifferentiable
import starling.curves.VolKey
import starling.curves.SpreadAtmStdDevAtomicDatumKey
import starling.market.{FuturesSpreadMarket, ForwardPriceRiskFactorType, Market, FuturesMarket}

case class CommoditySpreadOption(
        market: FuturesSpreadMarket,
        period: DateRange,
        strike: Quantity,
        volume: Quantity,
        callPut: CallOrPut
        ) extends SpreadOption(market, DateRangePeriod(period), strike, volume, callPut) {
  override def expiryDay = Some(market.spreadOptionExpiry(period))

  private def month = period.asInstanceOf[Month]

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(
    SingleCommoditySpreadOption(market, market.spreadOptionExpiry(period), month, strike, volume.copy(value = 1.0), callPut) -> volume.value
  ))

  def tradeableType = CommoditySpreadOption

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    ci.copy(index = Some(Right(market)), averagingPeriod = Some(period))
  }
}

case class SingleCommoditySpreadOption(
        market: FuturesSpreadMarket,
        exerciseDay: Day,
        month: Month,
        strike: Quantity,
        volume: Quantity,
        callPut: CallOrPut
        ) extends SingleSpreadOption(market, exerciseDay, DateRangePeriod(month), strike, volume, callPut) {
  def instrumentType = CommoditySpreadOption

  val valuationCCY = strike.uom * market.uom

  override def forwardState(env: Environment, dayAndTime: DayAndTime) = {
    if (exerciseDay.endOfDay > dayAndTime) {
      // exercise day after the forward day, still not expired
      this
    } else if (env.marketDay >= exerciseDay.endOfDay) {
      // already expired option, do nothing
      this
    } else {
      val exerciseDayEnv = env.forwardState(exerciseDay.endOfDay)
      val callPutSign = callPut match {case Call => 1.0; case Put => -1.0}
      val spread = exerciseDayEnv.spreadPrice(market, month)
      callPut.payoff(spread, strike) match {
        case Some(p) => new FuturesCommoditySpread(market, month, strike, volume * callPutSign)
        case None => this
      }
    }
  }

  def * (scale : Double) = copy(volume = volume * scale)

  def periodKey = Some(month)
  override def riskMarketExtra = String.format("%6.2f%n ", new java.lang.Double(strike.value)) + callPut.toShortString
  override def atomicKeyCachingUTP : UTP = copy(strike = 1.0(market.priceUOM))

  def price(env : Environment) = {
    val marketDay = env.marketDay.day
    val T = exerciseDay.endOfDay.timeSince(env.marketDay)
    val S = env.spreadPrice(market, month)
    val K = strike
    val discount = env.discount(valuationCCY, exerciseDay)
    if (T == 0){
      callPut.intrinsicPrice(S, K) * discount
    } else {
      val annualisedStdDev = env.spreadStdDev(market, month, exerciseDay, strike)
      val undiscountedPriceValue = new SpreadOptionCalculations(callPut, S.value, K.value, annualisedStdDev.checkedValue(market.priceUOM), 0.0, T).undiscountedPrice
      val undiscountedPrice = Quantity(undiscountedPriceValue, market.priceUOM)
      undiscountedPrice * discount
      Quantity.NULL
    }
  }
}

object CommoditySpreadOption extends InstrumentType[SingleCommoditySpreadOption] with TradeableType[CommoditySpreadOption] {
  val name = "Commodity Spread Option"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    create(row, row.getQuantity("Quantity"))
  }

  def create(row: RichInstrumentResultSetRow, volume: Quantity) = {
    val period = row.getDateRange("Period", Some(Month))
    new CommoditySpreadOption(row.getFuturesSpreadMarket("Market"), period, row.getQuantity("Strike"), volume, row.getCallPut("CallPut"))
  }

  def sample = {
    import starling.quantity.UOM._
    new CommoditySpreadOption(FuturesSpreadMarket.ICE_WTI_BRENT, Month(2010, 10), Quantity(0.2, USD / BBL), Quantity(10000, BBL), Call)
  }
}
