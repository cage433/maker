package starling.services

import starling.utils.ImplicitConversions._
import swing.event.Event
import collection.mutable.ListBuffer
import starling.gui.api._
import starling.gui.api.MarketDataSelection
import starling.utils.{Broadcaster, Receiver}
import starling.db.MarketDataStore

trait MarketDataAvailabilityChecker {
  def dataAvailable(source: MarketDataEventSource): Boolean
  def await(source: MarketDataEventSource)
}

class MarketDataAvailabilityBroadcaster(marketDataStore: MarketDataStore, broadcaster: Broadcaster, sources: List[MarketDataEventSource])
  extends MarketDataAvailabilityChecker with Receiver {

  private val waitiingSources = new ListBuffer[MarketDataEventSource]; waitiingSources ++= sources

  def event(event: Event): Unit = event partialMatch {
    case PricingGroupMarketDataUpdate(pricingGroup, newVersion, previousVersion, affectedObservationDays) => {
      val changesByDataFlow = waitingDataEventSourcesFor(pricingGroup)
        .toMapWithValues(_.changesFor(previousVersion, newVersion, affectedObservationDays))
        .filterValues(_.size > 0)

      waitiingSources -- changesByDataFlow.keySet

      if (changesByDataFlow.nonEmpty) {
        val marketDataIdentifier = MarketDataIdentifier(MarketDataSelection(Some(pricingGroup)), newVersion)
        val snapshot:SnapshotIDLabel = marketDataStore.snapshot(marketDataIdentifier, SnapshotType.Email).label
        changesByDataFlow.foreach{ case(source, changes) => {
          changes.foreach { change => {
            change.marketDataEvent(snapshot).foreach { realEvent => broadcaster.broadcast(realEvent) }
          }}
        } }
      }
    }
  }

  def dataAvailable(source: MarketDataEventSource) = { validateAreMonitoring(source); !waitiingSources.contains(source) }
  def await(source: MarketDataEventSource) =         { validateAreMonitoring(source); waitiingSources += source         }

  private def waitingDataEventSourcesFor(pricingGroup: PricingGroup) =
    waitiingSources.filter(source => source.matches(pricingGroup)).toList

  private def validateAreMonitoring(source: MarketDataEventSource) = if (!sources.contains(source)) {
    throw new Exception("The availability of: %s is not being monitored" % source)
  }
}