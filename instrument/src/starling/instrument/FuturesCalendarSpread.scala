package starling.instrument

import starling.curves.{Environment}
import starling.market.FuturesMarket
import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.UOM._
import starling.market.{Market, FuturesMarket}
import starling.quantity.{SpreadQuantity, UOM, Quantity}
import starling.curves.SpreadAtmStdDevAtomicDatumKey
import starling.curves.FuturesSpreadPrice
import starling.daterange._
import starling.quantity.NamedQuantity

case class FuturesCalendarSpread(market: FuturesMarket, firstMonth: Month, secondMonth: Month, firstStrike: Quantity, secondStrike: Quantity, volume: Quantity)
        extends UTP with Tradeable with AsUtpPortfolio with MultiLeg {
  assert(firstStrike.uom == secondStrike.uom, "Strike units should be the same: " + (firstStrike, secondStrike))
  require(firstStrike.denominatorUOM == market.uom, "Can't handle strike in non-market uom, strike: " + firstStrike + ", market: " + market.uom)
  require(volume.uom == market.uom, "Can't handle volume in non-market uom, volume: " + volume + ", market: " + market.uom)

  def this(market: FuturesMarket, firstMonth: Month, secondMonth: Month, spread: Quantity, volume: Quantity) {
    this(market, firstMonth, secondMonth, spread.copy(value = 0), -spread, volume)
  }

  override def expiryDay() = Some(market.lastTradingDay(secondMonth))

  val frontFuture: Future = Future(market, firstMonth, firstStrike, volume)
  val backFuture: Future = Future(market, secondMonth, secondStrike, volume.negate)

  def *(x : Double) = copy(volume = volume * x)

  def isLive(dayAndTime: DayAndTime) = backFuture.isLive(dayAndTime)

  def valuationCCY = frontFuture.valuationCCY

  def assets(env: Environment) = frontFuture.assets(env) ++ backFuture.assets(env)

  def explanation(env : Environment) : NamedQuantity = {
    frontFuture.explanation(env).named("Front") + backFuture.explanation(env).named("Back")
  }
  
  def asUtpPortfolio(tradeDay:Day) = {
    frontFuture.asUtpPortfolio ++
            backFuture.asUtpPortfolio
  }

  def legs = List(frontFuture, backFuture)

  def instrumentType = FuturesCalendarSpread

  def persistedTradeableDetails = Map("Market" -> market, "Period" -> Spread(firstMonth, secondMonth), "InitialPrice" -> SpreadQuantity(firstStrike, secondStrike), "Quantity" -> volume)

  def tradeableType = FuturesCalendarSpread

  def daysForPositionReport(marketDay: DayAndTime) = List(market.lastTradingDay(firstMonth).min(firstMonth.firstDay))


  override def priceAndVolKeys(marketDay : DayAndTime) = {
    var (pk, _) = super.priceAndVolKeys(marketDay)
    pk = if (pk.isEmpty) pk else Set(FuturesSpreadPrice(market, firstMonth / secondMonth))
    (pk, Set())
  }

  def periodKey = Some(SpreadPeriod(firstMonth, secondMonth))

  def price(env : Environment) = {
    env.forwardPrice(market, firstMonth) - env.forwardPrice(market, secondMonth)
  }
}

object FuturesCalendarSpread extends InstrumentType[FuturesCalendarSpread] with TradeableType[FuturesCalendarSpread] {

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val market = row.getFuturesMarket("market")
    val period = row.getSpread("period")
    val strikes = row.getStrikes
    val volume = row.getQuantity("Quantity")
    new FuturesCalendarSpread(market, period.first, period.last, strikes.front, strikes.back, volume)
  }

  def sample = new FuturesCalendarSpread(Market.NYMEX_WTI, Month(2012, 1), Month(2012, 2), Quantity(0, USD/BBL), Quantity(1, BBL))

  val name = "Futures Calendar Spread"
}
