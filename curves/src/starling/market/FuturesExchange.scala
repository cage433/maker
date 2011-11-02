package starling.market

import java.lang.String
import starling.daterange.ObservationTimeOfDay._
import starling.utils.StarlingEnum
import starling.utils.ImplicitConversions._
import starling.daterange.{Day, ObservationTimeOfDay}
import starling.marketdata.AreaCode
import scalaz.Scalaz._


trait DeliveryType {
  val name: String
  override def toString = name
}
case object DailyDelivery extends DeliveryType {
  val name = "Daily Delivery"
}
case object MonthlyDelivery extends DeliveryType {
  val name = "Monthly Delivery"
}

case class FuturesExchange(name: String, deliveryType: DeliveryType, closeTime:ObservationTimeOfDay, fixingLevel : Level = Level.Close) {
  // The presence of 'London Close' markets meant that the pair exchange/commodity could map to more than one market
  private lazy val futuresMarkets: List[FuturesMarket] = Market.futuresMarkets.filterNot(_.name.contains("London close"))
  lazy val markets = Market.marketsWithExchange(this)
  lazy val marketsByCommodityName = markets.toMapWithKeys(_.commodity.name.toLowerCase)
  def inferMarketFromCommodity(commodity : Commodity) : Option[FuturesMarket] = futuresMarkets.filter(_.exchange == this).find(_.commodity == commodity)
  def limName = (this == FuturesExchangeFactory.SFS) ? "SHFE" | name
}


object FuturesExchange{
  def fromNeptuneCode(code : String) : FuturesExchange = Map(
    "CMX" → FuturesExchangeFactory.COMEX,
    "LME" → FuturesExchangeFactory.LME,
    "SFS" → FuturesExchangeFactory.SFS
  ).get(code) match {
    case Some(exchange) => exchange
    case None => throw new IllegalStateException("No exchange for Neptune code " + code)
  }
  def fromArea(area : AreaCode) : Option[FuturesExchange] = {
    import AreaCode._ 

    area partialMatch {
      case `EUR` => FuturesExchangeFactory.LME
      case `ASI` | `CHN` => FuturesExchangeFactory.SFS
      case `SAM` | `NAM` => FuturesExchangeFactory.COMEX
    }
  }
}

object FuturesExchangeFactory extends StarlingEnum(classOf[FuturesExchange], (f: FuturesExchange) => f.name, otherTypes = Nil) {
  val LME = new FuturesExchange("LME", DailyDelivery, LMEClose) {
    lazy val calendar = Market.cals.LME

    /**
     * The month dates follows something like this rule
     * 1. Shift today's month by 'nMonthsAhead'
     * 2. If the date produced has a day number greater than the last day of the month (e.g. 30th Feb), then move to the month's last day
     * 3. If the day is a non-working day then
     *  a) For Friday/Saturday (bank holiday) - move to the previous business day
     *  b) For Sunday/Monday (bank holiday) - move to the next business day, unless this would move us to the next month in which case move back
     *  c) Any other holiday - just move back
     *
     * These are based on a chat with the LME, the holiday behaviour I've made up - but not the weekends.
     * TODO - put in exact rule once we've received the copy of the regs from the LME
     */
    def monthDate(marketDay : Day, nMonthsAhead : Int) : Day = {
      val month = marketDay.containingMonth + nMonthsAhead
      val d : Int = month.lastDay.dayNumber min marketDay.dayNumber
      val firstBusDayInMonth = calendar.thisOrNextBusinessDay(month.firstDay)
      val lastBusDayInMonth = calendar.thisOrPreviousBusinessDay(month.lastDay)

      var day = Day(month.y, month.m, d)
      if (!calendar.isBusinessDay(day)) {
        if (day.isSaturday || day.isFriday)
          day = calendar.previousBusinessDay(day)
        else if (day.isSunday || day.isMonday)
          day = calendar.nextBusinessDay(day)
        else
          day = calendar.previousBusinessDay(day)
      }
      (day max firstBusDayInMonth) min lastBusDayInMonth
    }

    def threeMonthDate(marketDay : Day) = monthDate(marketDay, nMonthsAhead = 3)
    def twoMonthDate(marketDay : Day) = monthDate(marketDay, nMonthsAhead = 2)

  }

  val COMEX = new FuturesExchange("COMEX", MonthlyDelivery, COMEXClose) {
    lazy val calendar = Market.cals.COMEX
  }
  val NYMEX = new FuturesExchange("NYMEX", MonthlyDelivery, Default) {
    lazy val calendar = Market.cals.NYMEX
  }
  val SFS = new FuturesExchange("SFS", MonthlyDelivery, SHFEClose, fixingLevel = Level.Settle) {
    lazy val calendar = Market.cals.SFS
  }
    
  val BALTIC = new FuturesExchange("Baltic Exchange", MonthlyDelivery, Default)
  val ICE = new FuturesExchange("ICE", MonthlyDelivery, Default)
  val MDEX = new FuturesExchange("MDEX", MonthlyDelivery, Default) // Malaysia Derivatives Exchange
  val TOCOM = new FuturesExchange("TOCOM", MonthlyDelivery, Default) // Tokyo Commodity Exchange
  val DCE = new FuturesExchange("DCE", MonthlyDelivery, Default) // Dalian Commodity Exchange
  val DME = new FuturesExchange("DME", MonthlyDelivery, Default) // Dubai Mercantile Exchange
  val EXBXG = new FuturesExchange("EXBXG", MonthlyDelivery, Default) // China Stainless Steel Exchange
  val NYSE = new FuturesExchange("NYSE", MonthlyDelivery, Default)
}
