package starling.instrument

import starling.quantity.{Quantity, UOM, Percentage}
import starling.richdb.RichInstrumentResultSetRow
import starling.curves._
import starling.market._
import rules.{NoPricingRule, CommonPricingRule, SwapPricingRule}
import starling.daterange._
import starling.quantity.NamedQuantity
import starling.quantity.BinOpNamedQuantity

/**
 * A SwapCalendarSpread is on a single index between 2 periods.
 */
case class SwapCalendarSpread(index: SingleIndex,
                              strike: Quantity,
                              volume: Quantity,
                              period: SpreadPeriod,
                              cleared: Boolean)
  extends Swap(index, strike, volume, period, cleared, pricingRule = NoPricingRule) with UTP {

  override def expiryDay() = Some(period.back.lastDay)

  private val settlementDay = CommoditySwap.swapSettlementDate(period.back.lastDay)
  val front = new SinglePeriodSwap(index, strike, volume, period.front, cleared = true, settlementDayOption = Some(settlementDay))
  val back = new SinglePeriodSwap(index, strike.copy(value = 0.0), -volume, period.back, cleared = true, settlementDayOption = Some(settlementDay))

  def explanation(env : Environment) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val discount = if (cleared) new Quantity(1.0) else namedEnv.discount(valuationCCY, settlementDay)
    (price(namedEnv).asInstanceOf[NamedQuantity] - strike.named("K")) * volume.named("Volume") * discount
  }

  def asUtpPortfolio(tradeDay: Day) = {
    UTP_Portfolio(
      Map(
        front.copy(volume = front.volume.copy(value = 1.0)) -> front.volume.value, 
        back.copy(volume = back.volume.copy(value = 1.0)) -> back.volume.value
      )
    )
  }

  def tradeableType = SwapCalendarSpread

  def assets(env: Environment) = asUtpPortfolio(env.marketDay.day).assets(env)

  def price(env: Environment) = front.price(env) - back.price(env)

  def periodKey = Some(period)

  def *(scale: Double) = copy(volume = volume * scale)

  def daysForPositionReport(marketDay: DayAndTime) = {
    val averagingDays = period.toList.flatMap(p => p.days.filter(index.isObservationDay(_)))
    val liveAveragingDays = averagingDays.filter(_.endOfDay > marketDay)

    if (liveAveragingDays.isEmpty)
      List(averagingDays.last)
    else
      liveAveragingDays
  }

  def detailsForUTPNOTUSED = super.persistedTradeableDetails

  def instrumentType = SwapCalendarSpread

  override def priceAndVolKeys(marketDay: DayAndTime) = {
    (asUtpPortfolio(marketDay.day).instruments.flatMap(_.priceAndVolKeys(marketDay)._1), Set())
  }
}

object SwapCalendarSpread extends InstrumentType[SwapCalendarSpread] with TradeableType[SwapCalendarSpread] {
  val name = "Swap Calendar Spread"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val index = row.getIndexFromName("Market").asInstanceOf[SingleIndex]
    val period = row.getPeriod("Period") match {
      case p:SpreadPeriod => p
      case u => throw new Exception("Unrecognised period for SwapCalendarSpread: " + u)
    }
    SwapCalendarSpread(index, row.getQuantity("InitialPrice"), row.getQuantity("Quantity"), period, cleared = true)
  }

  def sample = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    SwapCalendarSpread(Index.DATED_BRENT, 123(USD/BBL), 77000(BBL), SpreadPeriod(Month(2015, 1), Month(2015, 1)), cleared = true)
  }
}
