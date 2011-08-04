package starling.services

import starling.gui.api._
import starling.market.FuturesExchange

case class DataFlow(exchange: FuturesExchange, pricingGroup: PricingGroup, markets: List[String], source: String, from: String, to: String) {
  val sink = pricingGroup.name + '.' + exchange.name
}