package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{EmailEvent, MarketDataSelection, PricingGroup}
import starling.pivot._
import starling.market.FuturesExchange
import starling.marketdata.PriceDataType
import starling.utils.Broadcaster
import starling.utils.ImplicitConversions._


class VerifyPriceAvailable(marketDataStore: MarketDataStore, pricingGroup: PricingGroup, exchange: FuturesExchange,
                           broadcaster: Broadcaster, from: String, to: String*)
  extends EmailingScheduledTask(broadcaster, from, to) {

  private val pfs = PivotFieldsState(dataFields = fields("Market", "Price"), rowFields = fields("Period"))

  def eventFor(observationDay: Day, email: EmailEvent): Option[EmailEvent] = {
    val filter = filters("Exchange" → exchange.name, "Observation Day" → observationDay)

    val dataSource = marketDataStore.pivot(MarketDataSelection(Some(pricingGroup)), PriceDataType)
    val prices = dataSource.data(pfs.copy(filters = filter))

    (prices.numberOfRows == 0).toOption {
      email.copy(subject = "Missing Prices for: %s" % filterToString(filter),
                    body = "<p>No Prices have been uploaded for %s</p>" % filterToString(filter))
    }
  }
}