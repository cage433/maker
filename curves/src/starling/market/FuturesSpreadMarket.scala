package starling.market

import formula.Formula
import starling.quantity.UOM
import starling.utils.StarlingEnum
import starling.calendar.BusinessCalendar
import starling.daterange.{Day, DateRange, TenorType}

/**
 * Whatever the name the price is calculated by market1 - market2.
 * The formula is sanity checked to make sure it states the same thing.
 */
case class FuturesSpreadMarket(name: String, uom: UOM, ccy: UOM,
                               formula: Formula,
                               lotSize: Double, tenor: TenorType,
                               eaiQuoteID: Option[Int],
                               optionExpiryRule: Option[FuturesExpiryRule])
  extends Market with KnownExpiry {

  lazy val markets = formula.indexes.flatMap {
    case i: FuturesFrontPeriodIndex if i.promptness == 1 => Some(i.market)
    case _ => None
  }.toList

  lazy val (market1 :: market2 :: Nil) = {
    import starling.quantity.UOM._
    import starling.quantity.RichQuantity._

    val (index1 :: index2 :: Nil) = formula.indexes.toList
    
    assert(formula.price(USD) {
      case `index1` => 5.0(USD)
      case `index2` => 2.0(USD)
    } == 3.0(USD)) // just a sanity check to make sure the formula is market1 - market2

    markets
  }

  override def toString = name

  lazy val hasOptions: Boolean = optionExpiryRule.isDefined

  lazy val businessCalendar = throw new Exception("No calendar for " + this)

  lazy val expiryRule = optionExpiryRule match {
    case Some(er) => er
    case None => new FuturesExpiryRule {
      def lastTradingDay(d: DateRange) = market1.lastTradingDay(d) max market2.lastTradingDay(d)

      val name = "Spread Expiry Rule"
    }
  }

  val uomName = uom.toString

  def priceUOM = ccy / uom

  def premiumSettlementDay(tradeDay: Day) = tradeDay.addBusinessDays(businessCalendar, 5)
}

object FuturesSpreadMarket {
  lazy val provider = MarketProvider.provider

  /**
   * Futures Commodity Spread Markets
   */
  lazy val RB_CRACKS = futuresMarketFromName("Nymex RBOB vs Nymex WTI")
  lazy val RB_BRENT_CRACKS = futuresMarketFromName("NYMEX RBOB 1st Month vs IPE Brent 1st Month")
  lazy val RBHO = futuresMarketFromName("Nymex RBOB vs Nymex Heat")
  lazy val GO_CRACKS = futuresMarketFromName("IPE Gas Oil (Settlement) vs IPE Brent")
  lazy val ICE_WTI_BRENT = futuresMarketFromName("ICE WTI 1st month vs ICE Brent 1st month")
  lazy val NYMEX_WTI_BRENT = futuresMarketFromName("NYMEX WTI vs IPE Brent")

  def futuresMarketFromName(marketName: String): FuturesSpreadMarket = futuresMarketFromNameOption(marketName).getOrElse(throw new Exception("No market with name: " + marketName))
  def futuresMarketFromNameOption(marketName: String): Option[FuturesSpreadMarket] = provider.futuresSpreadMarket(marketName)

  def futuresMarketFromQuoteID(id: Int): FuturesSpreadMarket = provider.futuresSpreadMarket(id).getOrElse(throw new Exception("No market: " + id))

  lazy val all = List(RB_BRENT_CRACKS)
}
