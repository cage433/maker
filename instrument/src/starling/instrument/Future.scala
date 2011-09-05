package starling.instrument

import starling.quantity.Quantity._
import starling.quantity.UOM._
import starling.utils.ImplicitConversions._
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange._
import starling.curves._
import starling.daterange.DateRangePeriod
import starling.market.{Index, FuturesFrontPeriodIndex, FuturesMarket, Market}
import starling.quantity.{SimpleNamedQuantity, NamedQuantity, Quantity}

/** A class which represents a futures trade
 */
case class Future(market: FuturesMarket, delivery: DateRange, strike: Quantity, volume: Quantity)
  extends UTP with Tradeable with HedgingTradeable {
  require(strike.denominatorUOM == volume.uom, "Can't handle strike in non-volume uom, strike: " + strike + ", volume: " + volume.uom)
  require(market.convert(volume, market.uom).isDefined, "Invalid volume UOM, can't convert to market uom: " + volume)

  def explanation(env : Environment) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val F = SimpleNamedQuantity("F", convertPrice(namedEnv, namedEnv.fixingOrForwardPrice(market, lastTradingDay, delivery)))
    (F - strike.named("K")) * volume.named("Volume")
  }
  
  def *(x : Double) = copy(volume = volume * x)

  def assets(env: Environment) = {
    val F = convertPrice(env, env.fixingOrForwardPrice(market, lastTradingDay, delivery))
    Assets(
      Asset.estimatedCash(env.marketDay.day, F * volume, F * volume),
      Asset.estimatedCash(env.marketDay.day, -strike * volume, -strike * volume)
    )
  }

  private def convertPrice(env: Environment, price: Quantity) = {
    var result = price
    if (market.currency != valuationCCY){
      // For cross currency futures I don't think we should ne using a forward price, i.e. multiplying by the forward fx rate,
      // because of novation. Multiplying by spot is the only thing that makes sense to me.
      result *= env.spotFXRate(valuationCCY, market.currency)
    }
    result = market.convertUOM(result, strike.uom)
    result
  }

  override def expiryDay() = Some(lastTradingDay)

  val valuationCCY = strike.numeratorUOM

  val lastTradingDay = market.lastTradingDay(delivery)

  def isLive(dayAndTime: DayAndTime) =  dayAndTime < lastTradingDay.endOfDay

  def instrumentType = Future
  def tradeableType = Future

  def persistedTradeableDetails :Map[String, Any] = Map("Market" -> market, "Period" -> delivery, "Initial Price" -> strike, "Quantity" -> volume)
  def detailsForUTPNOTUSED :Map[String, Any] = Map("Market" -> market, "Period" -> delivery)

  def asUtpPortfolio(tradeDay:Day) = asUtpPortfolio
  def asUtpPortfolio = UTP_Portfolio(
    Map(
      Future(market, delivery, Quantity(0.0, strike.uom), Quantity(1.0, volume.uom)) -> volume.value,
      BankAccount(1.0(valuationCCY), Some(market), None, delivery) -> (- strike * volume).value
      )
  )

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(market.lastTradingDay(delivery).min(delivery.firstDay))

  def periodKey = Some(DateRangePeriod(delivery))

  def price(env : Environment) = {
    if(isLive(env.marketDay))
      env.forwardPrice(market, delivery)
    else
      0 (market.priceUOM)
  }

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    ci.copy(index = Some(Right(market)), averagingPeriod = Some(delivery))
  }
}

object Future extends InstrumentType[Future] with TradeableType[Future] {
   val name = "Future"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val futuresMarket = row.getFuturesMarket("Market")
    Future(futuresMarket, row.getDateRange("Period", Some(futuresMarket.tenor)), row.getQuantity("InitialPrice"), row.getQuantity("Quantity"))
  }
  def sample = {
    val leadMarket = Market.LME_LEAD
    import starling.quantity.UOM._
    Future(leadMarket, Day(2009, 8, 3), 123(USD/MT), 550 (MT))
  }
}
