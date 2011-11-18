package starling.services

import starling.daterange.Day
import starling.db.{MarketDataEntry, MarketDataSet, MarketDataStore}
import starling.scheduler.ScheduledTask
import starling.utils.ImplicitConversions._
import starling.marketdata._
import starling.gui.api.{MarketDataSelection, PricingGroup}

case class CopyManualData(marketDataStore: MarketDataStore, dataTypes: MarketDataTypes) extends ScheduledTask {
  private val manualTypes = dataTypes.manualTypes.map(_.name)

  protected def execute(ignoreBecauseItMightNotBeToday: Day) = {
    val today = Day.today

    def maxObservationDay(marketDataType: MarketDataTypeName): Option[Day] =
      marketDataStore.latestObservationDayFor(PricingGroup.Metals, marketDataType).filter(_ < today)

    val entries = manualTypes.toMapWithSomeValues(maxObservationDay(_)).flatMap { case (marketDataType, lastObservationDay) => {
      val previousData = queryPreviousData(marketDataType, lastObservationDay)

      (lastObservationDay upto today).toList.tail.filter(_.isWeekday)
        .flatMap { dayToCopyTo => previousData.map(_.copyDay(dayToCopyTo)) }
    } }.toList

    marketDataStore.save(MultiMap(MarketDataSet.ManualMetals → entries))
  }

  private def queryPreviousData(marketDataType: MarketDataTypeName, observationDay: Day): List[MarketDataEntry] =
    marketDataStore.queryLatest(MarketDataSelection(Some(PricingGroup.Metals)), marketDataType, Some(Set(Some(observationDay))))
      .map { case (timedKey, marketData) => MarketDataEntry(timedKey.observationPoint, timedKey.key, marketData) }
}