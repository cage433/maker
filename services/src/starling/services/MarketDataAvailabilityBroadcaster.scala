package starling.services

import starling.db.MarketDataStore
import starling.utils.ImplicitConversions._
import starling.utils.ObservingBroadcaster
import swing.event.Event
import collection.mutable.ListBuffer
import collection.immutable.Map
import starling.gui.api._

trait MarketDataAvailabilityChecker {
  def dataAvailable(source: MarketDataEventSource): Boolean
  def await(source: MarketDataEventSource)
}

class MarketDataAvailabilityBroadcaster(marketDataStore: MarketDataStore, observingBroadcaster: ObservingBroadcaster, sources: List[MarketDataEventSource])
  extends MarketDataAvailabilityChecker {

  private val waitiingSources = new ListBuffer[MarketDataEventSource]; waitiingSources ++= sources

  observingBroadcaster += eventReceived

  def eventReceived(event: Event): Unit = event partialMatch {
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
            val realEvent = change.marketDataEvent(snapshot)
            observingBroadcaster.broadcaster.broadcast(realEvent)
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