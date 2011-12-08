package starling.services

import starling.utils.ImplicitConversions._
import swing.event.Event
import collection.mutable.ListBuffer
import starling.gui.api._
import starling.gui.api.MarketDataSelection
import starling.db.MarketDataStore
import starling.scheduler.ScheduledTask
import starling.databases.MarketDataEventSource
import starling.manager.{Receiver, Broadcaster}

trait MarketDataAvailabilityChecker {
  def dataAvailable(source: MarketDataEventSource): Boolean
  def await(source: MarketDataEventSource)
}

class MarketDataAvailabilityBroadcaster(marketDataStore: MarketDataStore, broadcaster: Broadcaster, sources: List[MarketDataEventSource])
  extends MarketDataAvailabilityChecker with Receiver {

  private val waitingSources = new ListBuffer[MarketDataEventSource]; waitingSources ++= sources

  def event(event: Event): Unit = event partialMatch {
    case PricingGroupMarketDataUpdate(pricingGroup, newVersion, previousVersion, affectedObservationDays) => {
      val changesByDataFlow = waitingDataEventSourcesFor(pricingGroup)
        .toMapWithValues(_.changesFor(previousVersion, newVersion, affectedObservationDays))
        .filterValues(_.size > 0)

      waitingSources -- changesByDataFlow.keySet

      if (changesByDataFlow.nonEmpty) {
        val marketDataIdentifier = MarketDataIdentifier(MarketDataSelection(Some(pricingGroup)), newVersion)
        val snapshot:SnapshotIDLabel = marketDataStore.snapshot(marketDataIdentifier, SnapshotType.MarketData, observationDay = None).label
        changesByDataFlow.foreach{ case(source, changes) => {
          changes.foreach { change => {
            change.eventFor(snapshot).foreach { realEvent => broadcaster.broadcast(realEvent) }
          }}
        } }
      }
    }
  }

  def dataAvailable(source: MarketDataEventSource) = { validateAreMonitoring(source); !waitingSources.contains(source) }
  def await(source: MarketDataEventSource) =         { validateAreMonitoring(source); waitingSources += source         }

  private def waitingDataEventSourcesFor(pricingGroup: PricingGroup) =
    waitingSources.filter(source => source.matches(pricingGroup)).toList

  private def validateAreMonitoring(source: MarketDataEventSource) = if (!sources.contains(source)) {
    throw new Exception("The availability of: %s is not being monitored" % source)
  }
}