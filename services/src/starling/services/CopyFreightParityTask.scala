package starling.services

import starling.utils.ImplicitConversions._
import starling.db.{MarketDataEntry, MarketDataSet, MarketDataStore}
import starling.daterange.Day
import starling.gui.api.{PricingGroup, MarketDataSelection}
import starling.marketdata.{GradeAreaBenchmarkDataType, CountryBenchmarkDataType, FreightParityDataType}


case class CopyManualData(marketDataStore: MarketDataStore) extends ScheduledTask {
  protected def execute(today: Day) = {
    val identifier = marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals)))
    val entries = List(new FreightParityDataType, new CountryBenchmarkDataType, new GradeAreaBenchmarkDataType).flatMap { marketDataType => {
      val lastObservationDay = marketDataStore.queryForObservationDayAndMarketDataKeys(identifier, marketDataType.name)
        .flatMap(_.observationPoint.day).optMax.filter(_ != today)

      lastObservationDay.toList.flatMap { observationDay => {
        val previousData = marketDataStore.query(identifier, marketDataType.name, Some(Set(Some(observationDay))))
        val newData = previousData.mapFirst(_.copyDay(today))
        newData.map { case (timedKey, marketData) => MarketDataEntry(timedKey.observationPoint, timedKey.key, marketData) }
      } }
    } }
    marketDataStore.save(MultiMap(MarketDataSet.ManualMetals → entries))
  }
}





