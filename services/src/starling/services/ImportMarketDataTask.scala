package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup}


class ImportMarketDataTask(marketDataStore: MarketDataStore, pricingGroup: PricingGroup) extends ScheduledTask {
  def execute(observationDay: Day) = marketDataStore.importData(MarketDataSelection(Some(pricingGroup)), observationDay)
}
