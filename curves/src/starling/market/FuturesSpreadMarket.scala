package starling.market

import formula.Formula
import starling.quantity.UOM
import starling.utils.StarlingEnum
import starling.calendar.BusinessCalendar
import starling.daterange.ObservationTimeOfDay._
import starling.market.FuturesSpreadMarket._
import starling.daterange.{Location, Day, DateRange, TenorType}

/**
 * Whatever the name the price is calculated by market1 - market2.
 */
class FuturesSpreadMarket(name: String, uom: UOM, ccy: UOM,
                               val market1: FuturesMarket,
                               val market2: FuturesMarket,
                               lotSize: Option[Double],
                               tenor: TenorType,
                               eaiQuoteID: Option[Int],
                               expiryRule: FuturesExpiryRule,
                               exchange: FuturesExchange,
                               hasOptions: Boolean
                                )
  extends FuturesMarket(name, lotSize, uom, ccy, null, eaiQuoteID, tenor, expiryRule, exchange, SpreadCommodity) {

  override def toString = name

  override val businessCalendar = new BusinessCalendar {
    def location = Location.Unknown

    def isHoliday(day: Day) = {
      market1.businessCalendar.isHoliday(day) && market2.businessCalendar.isHoliday(day)
    }

    def name = FuturesSpreadMarket.this.name + " calendar"
  }
}

object FuturesSpreadMarket {
  private def provider = MarketProvider.provider

  def defaultExpiryRule(market1: FuturesMarket, market2: FuturesMarket) = new FuturesExpiryRule {
    def lastTradingDay(d: DateRange) = market1.lastTradingDay(d) max market2.lastTradingDay(d)

    val name = "Spread Expiry Rule"
  }

  val NoExchangeMonthly = FuturesExchange("NoExchange", MonthlyDelivery, Default)

  val SpreadCommodity = new Commodity{}

  /**
   * Futures Commodity Spread Markets
   */
  lazy val RB_CRACKS = futuresMarketFromName("Nymex RBOB vs Nymex WTI")
  lazy val RB_BRENT_CRACKS = futuresMarketFromName("NYMEX RBOB 1st Month vs IPE Brent 1st Month (BBLS)")
  lazy val RBHO = futuresMarketFromName("Nymex RBOB vs Nymex Heat")
  lazy val GO_CRACKS = futuresMarketFromName("IPE Gas Oil (Settlement) vs IPE Brent")
  lazy val ICE_WTI_BRENT = futuresMarketFromName("ICE WTI 1st month vs ICE Brent 1st month")
  lazy val HO_BRENT = futuresMarketFromName("NYMEX Heat 1st Month vs ICE Brent 1st Month")
  lazy val NYMEX_WTI_BRENT = futuresMarketFromName("NYMEX WTI vs IPE Brent")

  def futuresMarketFromName(marketName: String): FuturesSpreadMarket = futuresMarketFromNameOption(marketName).getOrElse(throw new Exception("No market with name: " + marketName))

  def futuresMarketFromNameOption(marketName: String): Option[FuturesSpreadMarket] = provider.futuresSpreadMarket(marketName)

  def futuresMarketFromQuoteID(id: Int): FuturesSpreadMarket = provider.futuresSpreadMarket(id).getOrElse(throw new Exception("No market: " + id))
}
