package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.UOM._
import starling.quantity.{SpreadQuantity, UOM, Quantity}
import starling.daterange._
import starling.curves.{PriceDifferentiable, Environment, SpreadAtmStdDevAtomicDatumKey, FuturesSpreadPrice}
import starling.market.{Market, FuturesSpreadMarket}

case class FuturesCommoditySpread(market: FuturesSpreadMarket, month: Month, firstStrike: Quantity, secondStrike: Quantity, volume: Quantity)
        extends UTP with Tradeable with AsUtpPortfolio with MultiLeg {

  def this(market: FuturesSpreadMarket, month: Month, spread: Quantity, volume: Quantity) {
    this(market, month, spread.copy(value = 0), -spread, volume)
  }

  override def expiryDay() = Some(market.lastTradingDay(month))

  val future1: Future = Future(market.market1, month, firstStrike, volume)
  val future2: Future = Future(market.market2, month, secondStrike, volume.negate)

  def *(x : Double) = copy(volume = volume * x)

  def isLive(dayAndTime: DayAndTime) = future1.isLive(dayAndTime) && future2.isLive(dayAndTime)

  def valuationCCY = future1.valuationCCY

  def assets(env: Environment) = future1.assets(env) ++ future2.assets(env)

  def asUtpPortfolio(tradeDay:Day) = {
    future1.asUtpPortfolio ++
            future2.asUtpPortfolio
  }

  def legs = List(future1, future2)

  def instrumentType = FuturesCommoditySpread

  def detailsForUTPNOTUSED = persistedTradeableDetails - "Quantity"

  def persistedTradeableDetails = Map("Market" -> market, "Period" -> month, "InitialPrice" -> SpreadQuantity(firstStrike, secondStrike), "Quantity" -> volume)

  def tradeableType = FuturesCommoditySpread

  def daysForPositionReport(marketDay: DayAndTime) = List(market.market1.lastTradingDay(month).min(market.market2.lastTradingDay(month).min(month.firstDay)))

  def periodKey = Some(DateRangePeriod(month))

  def price(env : Environment) = {
    env.forwardPrice(market.market1, month) - env.forwardPrice(market.market2, month)
  }
}

object FuturesCommoditySpread extends InstrumentType[FuturesCommoditySpread] with TradeableType[FuturesCommoditySpread] {

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val market = row.getFuturesSpreadMarket("market")
    val month = row.getMonth("period")
    val strikes = row.getStrikes
    val volume = row.getQuantity("Quantity")
    new FuturesCommoditySpread(market, month, strikes.front, strikes.back, volume)
  }

  def sample = new FuturesCommoditySpread(FuturesSpreadMarket.RB_CRACKS, Month(2012, 2), Quantity(0, USD/BBL), Quantity(1, BBL))

  val name = "Futures Commodity Spread"
}
