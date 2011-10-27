package starling.curves.readers

import starling.daterange._
import starling.marketdata._
import starling.db.MarketDataStore
import starling.utils.Broadcaster
import starling.gui.api._
import scalaz.Scalaz._
import starling.scheduler.EmailingScheduledTask
import starling.utils.ImplicitConversions._


case class VerifyAnyMarketDataAvailable(name: String, marketDataStore: MarketDataStore, selection: MarketDataSelection,
  marketDataType: MarketDataTypeName, marketDataKeys: Set[MarketDataKey], broadcaster: Broadcaster, sender: String,
  recipient: String) extends EmailingScheduledTask(broadcaster, sender, recipient) {

  protected def eventFor(observationDay: Day, email: EmailEvent) = {
    queryLatest(observationDay).isEmpty.option(email.copy(subject = "No Prices for: %s on %s" % (name, observationDay)))
  }

  private def queryLatest(observationDay: Day) = {
    marketDataStore.queryLatest(selection, marketDataType, Some(Set(Some(observationDay))), None, Some(marketDataKeys))
  }
}