package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup}
import org.scalacheck.Prop.True

class ImportMarketDataTask(marketDataStore: MarketDataStore, pricingGroup: PricingGroup) extends ScheduledTask {
  def execute(observationDay: Day) = marketDataStore.snapshot(MarketDataSelection(Some(pricingGroup)), true, observationDay)
}
