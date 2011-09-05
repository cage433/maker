package starling.services

import starling.db.MarketDataStore
import starling.utils.ImplicitConversions._
import starling.utils.ObservingBroadcaster
import swing.event.Event
import collection.mutable.ListBuffer
import starling.gui.api.{MarketDataSnapshotSet, MarketDataSelection}


trait MarketDataAvailabilityChecker {
  def dataAvailable(source: MarketDataEventSource): Boolean
  def await(source: MarketDataEventSource)
}

class MarketDataAvailabilityBroadcaster(observingBroadcaster: ObservingBroadcaster, sources: List[MarketDataEventSource])
  extends MarketDataAvailabilityChecker {

  private val waitiingSources = new ListBuffer[MarketDataEventSource]; waitiingSources ++= sources

  observingBroadcaster += eventReceived

  def eventReceived(event: Event): Unit = event partialMatch {
    case MarketDataSnapshotSet(selection, previousSnapshot, newSnapshot, Some(affectedObservationDays)) => {
      val dataAvailableEventByDataFlow = waitingDataEventSourcesFor(selection)
        .toMapWithValues(_.eventsFor(previousSnapshot, newSnapshot, affectedObservationDays))
        .filterValues(_.size > 0)

      waitiingSources -- dataAvailableEventByDataFlow.keySet

      dataAvailableEventByDataFlow.values.flatten.map(dataAvailableEvent =>
        observingBroadcaster.broadcaster.broadcast(dataAvailableEvent))
    }
  }

  def dataAvailable(source: MarketDataEventSource) = { validateAreMonitoring(source); !waitiingSources.contains(source) }
  def await(source: MarketDataEventSource) =         { validateAreMonitoring(source); waitiingSources += source         }

  private def waitingDataEventSourcesFor(selection: MarketDataSelection) =
    waitiingSources.filter(source => source.matches(selection)).toList

  private def validateAreMonitoring(source: MarketDataEventSource) = if (!sources.contains(source)) {
    throw new Exception("The availability of: %s is not being monitored" % source)
  }
}