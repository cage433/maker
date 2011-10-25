package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup}
import starling.scheduler.ScheduledTask

class ImportMarketDataTask(marketDataStore: MarketDataStore, pricingGroup: PricingGroup) extends ScheduledTask {
  def execute(observationDay: Day) = marketDataStore.importData(MarketDataSelection(Some(pricingGroup)), observationDay)
}
