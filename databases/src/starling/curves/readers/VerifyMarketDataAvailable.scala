package starling.curves.readers

import starling.daterange._
import starling.marketdata._
import starling.db.MarketDataStore
import starling.gui.api._
import starling.scheduler.EmailingScheduledTask
import starling.services.EmailService


abstract class VerifyMarketDataAvailable(marketDataStore: MarketDataStore, selection: MarketDataSelection,
  marketDataType: MarketDataTypeName, marketDataKeys: Set[MarketDataKey], emailService: EmailService, template: Email)

  extends EmailingScheduledTask(emailService, template) {

  final protected def queryLatest(observationDay: Day): List[(TimedMarketDataKey, MarketData)] = {
    marketDataStore.queryLatest(selection, marketDataType, Some(Set(Some(observationDay))), None, Some(marketDataKeys))
  }
}