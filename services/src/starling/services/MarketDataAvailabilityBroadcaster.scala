package starling.services

import starling.utils.ImplicitConversions._
import swing.event.Event
import collection.mutable.ListBuffer
import starling.gui.api._
import starling.gui.api.MarketDataSelection
import starling.utils.{Broadcaster, Receiver}
import starling.db.MarketDataStore
import starling.scheduler.ScheduledTask
import starling.databases.MarketDataEventSource

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
        val snapshot:SnapshotIDLabel = marketDataStore.snapshot(marketDataIdentifier, SnapshotType.Email).label
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

  def verifyPricesAvailable(dataFlow: DataFlow with MarketDataEventSource) =
    ("Verify Price %s available" % dataFlow.sink) → VerifyPriceAvailable(this, broadcaster, dataFlow)

  def verifyFixingsAvailable(dataFlow: DataFlow with MarketDataEventSource) =
    ("Verify Fixing %s available" % dataFlow.sink) → ScheduledTask.Null

  private def waitingDataEventSourcesFor(pricingGroup: PricingGroup) =
    waitingSources.filter(source => source.matches(pricingGroup)).toList

  private def validateAreMonitoring(source: MarketDataEventSource) = if (!sources.contains(source)) {
    throw new Exception("The availability of: %s is not being monitored" % source)
  }
}