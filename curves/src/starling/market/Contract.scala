package starling.market

import starling.daterange.DateRange

/**
 * A contract is a market and a period, for example Contract(Nymex WTI, Dec10)
 */
trait Contract {
  val market: Market
  val period: DateRange
}

case class PriceContract(market: CommodityMarket, period: DateRange) extends Contract

case class VolContract(market: Market with HasImpliedVol, period: DateRange) extends Contract