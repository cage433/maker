package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.market.{Market, FuturesMarket}
import starling.quantity.RichQuantity._
import starling.quantity.UOM._
import starling.quantity.{SimpleNamedQuantity, Quantity}
import starling.daterange.DateRangePeriod._
import starling.daterange._
import starling.curves.{MissingMarketDataException, Environment}

case class TAS(market: FuturesMarket, delivery: DateRange, maturityDay: Day, volume: Quantity)
  extends UTP with Tradeable with HedgingTradeable {
  def tradeableType = TAS

  def persistedTradeableDetails :Map[String, Any] = Map("Market" -> market, "Period" -> delivery, "Maturity Day" -> maturityDay, "Quantity" -> volume)

  def isLive(dayAndTime: DayAndTime) = dayAndTime < lastTradingDay.endOfDay

  def valuationCCY = market.currency

  private def strike(env: Environment): Option[Quantity] = if (maturityDay.endOfDay <= env.marketDay) {
    Some(env.priceOnDay(market, delivery, maturityDay))
  } else {
    None
  }

  private def asFuture(env: Environment): Option[Future] = strike(env).map(K =>
    Future(market, delivery, K, volume)
  )

  def lastTradingDay = market.lastTradingDay(delivery)
  override def expiryDay() = Some(lastTradingDay)

  def explanation(env: Environment) = {
    val future = new Future(market, delivery, 0 (market.priceUOM), 1 (market.uom))
    val namedEnv = env.withNaming()
    val forwardPrice = future.underlyingPrice(namedEnv)
    val F = SimpleNamedQuantity("F", forwardPrice)
    val K = strike(namedEnv) match {
      case None => new SimpleNamedQuantity("Floating(Fixes." + maturityDay + ")", forwardPrice)
      case Some(k) => k.named("K")
    }
    (F - K) * volume.named("Volume")
  }

  def assets(env: Environment) = asFuture(env) match {
    case Some(f) => f.assets(env)
    case None => {
      try {
         // ignore result but gives us a sensitiviy (of 0.0) to the underlying market and period
        // which looks nicer in the pivot reports as you can have a risk market and period that aren't blank.
        env.forwardPrice(market, delivery)
      } catch {
        case m:MissingMarketDataException =>
      }
      Assets(Asset.knownCash(env.marketDay.day, 0(valuationCCY), env))
    }
  }

  def instrumentType = TAS

  def daysForPositionReport(marketDay: DayAndTime) = List(market.lastTradingDay(delivery).min(delivery.firstDay))

  def *(x : Double) = copy(volume = volume * x)

  def periodKey = Some(DateRangePeriod(delivery))

  def price(env : Environment) = {
    if(isLive(env.marketDay))
      env.forwardPrice(market, delivery)
    else
      0 (market.priceUOM)
  }

  def asUtpPortfolio(tradeDay: Day) = asUtpPortfolio

  def asUtpPortfolio = UTP_Portfolio(
    Map(
      TAS(market, delivery, maturityDay, Quantity(1.0, volume.uom)) -> volume.value
    )
  )
}


object TAS extends InstrumentType[TAS] with TradeableType[TAS] {
   val name = "TAS"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val futuresMarket = row.getFuturesMarket("Market")
    TAS(futuresMarket, row.getDateRange("Period", Some(futuresMarket.tenor)), row.getDay("maturityDay"), row.getQuantity("Quantity"))
  }
  def sample = {
    TAS(Market.NYMEX_WTI, Month(2011, 2), Day(2011, 1, 1), 10000.0 (BBL))
  }
}