package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.pivot._
import starling.marketdata.PriceDataType
import starling.utils.Broadcaster
import starling.utils.ImplicitConversions._
import starling.gui.api.{EmailEvent, MarketDataSelection}


case class VerifyPriceAvailable(marketDataStore: MarketDataStore, broadcaster: Broadcaster, dataFlow: DataFlow)
  extends EmailingScheduledTask(broadcaster, dataFlow.from, dataFlow.to) {

  override def attributes = super.attributes +
    (DataSource → ScheduledTaskAttribute(dataFlow.source)) + (DataSink → ScheduledTaskAttribute(dataFlow.sink))

  private val pfs = PivotFieldsState(dataFields = fields("Market", "Price"), rowFields = fields("Period"))

  def eventFor(observationDay: Day, email: EmailEvent): Option[EmailEvent] = {
    val filter = addMarkets(filters("Exchange" → dataFlow.exchange.name, "Observation Day" → observationDay))

    val dataSource = marketDataStore.pivot(MarketDataSelection(Some(dataFlow.pricingGroup)), PriceDataType)
    val prices = dataSource.data(pfs.copy(filters = filter))

    (prices.numberOfRows == 0).toOption {
      email.copy(subject = "Missing Prices for: %s" % filterToString(filter),
                    body = "<p>No Prices have been uploaded for %s</p>" % filterToString(filter))
    }
  }

  private def addMarkets(filter: List[(Field, Selection)]): List[(Field, Selection)] = {
    if (dataFlow.markets.isEmpty) filter else (Field("Market") → SomeSelection(dataFlow.markets.toSet)) :: filter
  }
}