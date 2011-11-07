package starling.services

import starling.daterange.Day
import starling.db.{MarketDataEntry, MarketDataSet, MarketDataStore}
import starling.gui.api.{MarketDataSelection, PricingGroup}
import starling.marketdata.{CountryBenchmarkDataType, FreightParityDataType, GradeAreaBenchmarkDataType, ShanghaiVATDataType}
import starling.scheduler.ScheduledTask
import starling.utils.ImplicitConversions._


case class CopyManualData(marketDataStore: MarketDataStore) extends ScheduledTask {
  protected def execute(today: Day) = {
    val identifier = marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals)))
    val today = Day.today
    val entries = List(new FreightParityDataType, new CountryBenchmarkDataType, new GradeAreaBenchmarkDataType, ShanghaiVATDataType).flatMap { marketDataType => {
      marketDataStore.queryForObservationDayAndMarketDataKeys(identifier, marketDataType.name)
      .flatMap(_.observationPoint.day).optMax.toList.flatMap { lastObservationDay => {
        if (lastObservationDay < today) {
          val previousData = marketDataStore.query(identifier, marketDataType.name, Some(Set(Some(lastObservationDay))))
          (lastObservationDay upto today).toList.tail.filter(_.isWeekday).flatMap { dayToCopyTo => {
            val newData = previousData.mapFirst(_.copyDay(dayToCopyTo))
            newData.map { case (timedKey, marketData) => MarketDataEntry(timedKey.observationPoint, timedKey.key, marketData) }
          } }
        } else {
          Nil
        }
      } }
    }}
    marketDataStore.save(MultiMap(MarketDataSet.ManualMetals → entries))
  }
}





