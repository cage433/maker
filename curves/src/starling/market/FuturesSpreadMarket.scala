package starling.market

import starling.quantity.UOM
import starling.quantity.UOM._
import starling.utils.StarlingEnum

case class FuturesSpreadMarket(name: String, market1: FuturesMarket, market2: FuturesMarket, priceUOM: UOM, lotSize: Double) {
  override def toString = name
}

object FuturesSpreadMarket extends StarlingEnum(classOf[FuturesSpreadMarket], (f: FuturesSpreadMarket) => f.name, true) {
  import Market._

  /**
   * Futures Commodity Spread Markets
   */
  lazy val RB_CRACKS = new FuturesSpreadMarket("RB Cracks", NYMEX_GASOLINE, NYMEX_WTI, USD/BBL, 1000)
  lazy val RB_BRENT_CRACKS = new FuturesSpreadMarket("RB Brent Cracks", NYMEX_GASOLINE, ICE_BRENT, USD/BBL, 1000)
  lazy val ICE_WTI_BRENT= new FuturesSpreadMarket("WTI Brent", ICE_WTI, ICE_BRENT, USD/BBL, 1000)
  lazy val NYMEX_WTI_BRENT = new FuturesSpreadMarket("NY WTI Brent", NYMEX_WTI, ICE_BRENT, USD/BBL, 1000)
  lazy val GO_CRACKS = new FuturesSpreadMarket("GO Cracks", ICE_GAS_OIL, ICE_BRENT, USD/MT, 100)
  lazy val RBHO = new FuturesSpreadMarket("RBHO", NYMEX_GASOLINE, NYMEX_HEATING, USD/GAL, 42000)
}