package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.props.{Props, PropsHelper}
import starling.marketdata.PriceDataType
import starling.market.FuturesExchange
import starling.gui.api.{EmailEvent, MarketDataSelection, PricingGroup}
import starling.utils.Broadcaster


class VerifyPriceAvailable(marketDataStore: MarketDataStore, pricingGroup: PricingGroup, exchange: FuturesExchange,
                           broadcaster: Broadcaster, from: Props => PropsHelper#Property, to: (Props => PropsHelper#Property)*)
  extends BroadcastingScheduledTask(broadcaster) {

  private val email = EmailEvent(from = from(PropsHelper.defaultProps).value)
  private val pfs = PivotFieldsState(dataFields = fields("Market", "Price"), rowFields = fields("Period"))

  def eventFor(observationDay: Day): Option[EmailEvent] = {
    val filter = filters("Exchange" → exchange.name, "Observation Day" → observationDay)

    val dataSource = marketDataStore.pivot(MarketDataSelection(Some(pricingGroup)), PriceDataType)
    val prices = dataSource.data(pfs.copy(filters = filter))

    (prices.numberOfRows == 0).toOption {
      email.copy(to = to.map(f => f(PropsHelper.defaultProps).value),
                 subject = "Missing Prices for: %s" % filterToString(filter),
                 body = "<p>No Prices have been uploaded for %s</p>" % filterToString(filter))
    }
  }
}
