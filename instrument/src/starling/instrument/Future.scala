package starling.instrument

import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.quantity.UOM._
import starling.utils.ImplicitConversions._
import Math._
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange._
import starling.market.{FuturesFrontPeriodIndex, FuturesMarket, Market}
import starling.curves._
import starling.daterange.DateRangePeriod

/** A class which represents a futures trade
 */
case class Future(market: FuturesMarket, delivery: DateRange, strike: Quantity, volume: Quantity)
  extends UTP with Tradeable with HedgingTradeable {
  require(strike.denominatorUOM == volume.uom, "Can't handle strike in non-volume uom, strike: " + strike + ", volume: " + volume.uom)
  require(market.convert(volume, market.uom).isDefined, "Invalid volume UOM, can't convert to market uom: " + volume)

  def *(x : Double) = copy(volume = volume * x)

  def assets(env : Environment) = if (env.marketDay < lastTradingDay.endOfDay) {
    val F = convertPrice(env, env.forwardPrice(market, delivery))
    Assets(
      Asset.estimatedCash(env.marketDay.day, F * volume, F * volume),
      Asset.estimatedCash(env.marketDay.day, -strike * volume, -strike * volume)
    )
	} else {
    //The cash for futures after the last trading day is probably wrong
    //as the settlement day is market day
    //maybe we should try to create the correct margining payments using the fixings?
    //the trouble is the trade day becomes a valuation parameter 
    var fixings = convertPrice(env, env.indexFixing(FuturesFrontPeriodIndex(market), lastTradingDay))
    Assets(
      Asset.estimatedCash(env.marketDay.day, fixings * volume, env),
      Asset.estimatedCash(env.marketDay.day, -strike * volume, env) //strike is always zero because of the utps
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

  def tradeableDetails :Map[String, Any] = Map("Market" -> market, "Period" -> delivery, "Initial Price" -> strike, "Quantity" -> volume)
  def details :Map[String, Any] = Map("Market" -> market, "Period" -> delivery)

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
    env.forwardPrice(market, delivery)
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
