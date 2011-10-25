package starling.services

import starling.gui.api._
import starling.market.FuturesExchange

case class DataFlow(exchange: FuturesExchange, // is this valid for all data flows ?
                    pricingGroup: PricingGroup, markets: List[String],
                    from: String, to: String, source: String, description: String) {
  val sink = pricingGroup.name + '.' + exchange.name
}