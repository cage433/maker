package starling.services.rpc.valuation

import starling.rmi.RabbitEventDatabase
import com.trafigura.events.DemultiplexerClient
import starling.utils.Log
import starling.daterange.Day
import starling.db.SnapshotID
import com.trafigura.services.rabbit.Publisher
import com.trafigura.shared.events._
import org.codehaus.jettison.json.JSONArray
import com.trafigura.common.control.PipedControl._
import com.trafigura.edm.shared.types.TitanId
import starling.curves.{NullAtomicEnvironment, Environment}
import starling.services.rabbit.{EventPayloadFactory, EventFactory, TitanRabbitEventServices}
import starling.titan.{TitanTradeStoreManager, TitanTradeStore, TitanTacticalRefData}
import starling.instrument.physical.PhysicalMetalForward

/**
 * handler for Titan rabbit events
 */
class TitanEventHandler(rabbitEventServices : TitanRabbitEventServices,
                        titanTradeStoreManager : TitanTradeStoreManager,
                        environmentProvider : EnvironmentProvider,
                        db : RabbitEventDatabase) extends DemultiplexerClient with Log {


  import Event._

  lazy val rabbitPublishChangedValueEvents = publishStarlingChangedValueEvents(rabbitEventServices.rabbitEventPublisher)
  lazy val rabbitPublishNewValuationEvents = publishCreatedValuationEvents(rabbitEventServices.rabbitEventPublisher) _

  val EDMLogisticsQuotaSubject = "logistics quota" // todo, another constant that appears missing in the EDM model, needs raising to logistics, todo...

  /**
   * top level titan event handler
   */
  def handle(ev: Event) {
    log.debug("Received event " + ev)
    try {
      if (ev == null)
        log.warn("Got a null event")
      else {
        db.saveEvent(ev)
        if (TrademgmtSource == ev.source && (TradeSubject == ev.subject || NeptuneTradeSubject == ev.subject)) {
          // Must be some form of trade event from trademgmt source
          tradeMgmtTradeEventHander(ev)
        }
        else if (LogisticsSource.equalsIgnoreCase(ev.source) &&
          (EDMLogisticsSalesAssignmentSubject.equalsIgnoreCase(ev.subject) || EDMLogisticsInventorySubject.equalsIgnoreCase(ev.subject) || EDMLogisticsQuotaSubject.equalsIgnoreCase(ev.subject))) {
          logisticsInventoryEventHander(ev)
        }
        else if (StarlingSource.equalsIgnoreCase(ev.source) && StarlingMarketDataSnapshotIDSubject.equalsIgnoreCase(ev.subject)) {
          marketDataSnapshotEventHander(ev)
        }
      }
    } catch {
      case e =>
        log.warn("Error while processing event\n" +e.getStackTrace.mkString("\n"))
        throw e
    }
  }

  /**
   * handler for trademgmt trade events
   */
  def tradeMgmtTradeEventHander(ev: Event) = {
    log.info("handler: Got a trade event to process %s".format(ev.toString))

    val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
    val tradeIds = tradePayloads.map(p => p.key.identifier)
    val titanIds = tradeIds.map(id => TitanId(id))
    log.info("Trade event received for ids { %s }".format(tradeIds.mkString(", ")))
    val (snapshotIDString, env) = environmentProvider.recentRepresentativeEnvironment
    ev.subject match {
      case TradeSubject | NeptuneTradeSubject => {
        ev.verb match {
          case UpdatedEventVerb => {
            val changedIDs = tradeIds.filter(titanTradeStoreManager.updateTrade(env, _))
            if (changedIDs != Nil)
              rabbitPublishChangedValueEvents(changedIDs, RefinedMetalTradeIdPayload)

            log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
          }
          case CreatedEventVerb => {
            Log.info("New event received for %s".format(tradeIds))
            tradeIds.foreach(titanTradeStoreManager.updateTrade(env, _))
          }
          case CancelledEventVerb | RemovedEventVerb => {
            Log.info("Cancelled / deleted event received for %s".format(titanIds))
            tradeIds.foreach(titanTradeStoreManager.deleteTrade)
          }
        }
      }
      // handle publishing of valuation status events (events saying if a new (completed) trade can be valued successfully or not)
      case NeptuneTradeSubject => {
        val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier == "completed").size > 0
        if (completed) {
          val valuationResults : List[(String, Boolean)] = tradeIds.map {
            id =>
              (id, titanTradeStoreManager.titanTradeStore.getForward(id).costsAndIncomeQuotaValueBreakdown(env).isRight)
            }
          rabbitPublishNewValuationEvents(valuationResults)
        }
      }
    }
  }


  private def getID(payload : Payload) = payload.key.identifier

  /**
   * handler for logistics assignment events
   */
  def logisticsInventoryEventHander(ev: Event) = {
    log.info("handler: Got a logistics event to process %s".format(ev.toString))

    val payloads = ev.content.body.payloads
    val ids = if (Event.EDMLogisticsInventorySubject.equalsIgnoreCase(ev.subject)) {
      payloads.map(p => getID(p))
    } else if (EDMLogisticsQuotaSubject.equalsIgnoreCase(ev.subject)) {
      // todo, need to handle these ids...
      payloads.filter(p => "LogisticsQuota".equalsIgnoreCase(p.payloadType)).map(p => getID(p))
    }
    else Nil

    log.info("Logistics event received for ids { %s }".format(ids.mkString(", ")))


    // todo, need to remove logistics quota somehow here, since we can't reliably do it from inventory (can we?)

    val marketDay = Day.today.previousWeekday

    ev.verb match {
      case UpdatedEventVerb => {
        val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday match {
          case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId, marketDay))
          case None => ("No Snapshot found", Environment(NullAtomicEnvironment(marketDay.startOfDay)))
        }

        val changedForwardIDs = ids.flatMap(id => titanTradeStoreManager.updateInventory(env, id))

        if (changedForwardIDs != Nil) {
          rabbitPublishChangedValueEvents(changedForwardIDs, EDMLogisticsInventoryIdPayload)
        }

        log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedForwardIDs.size))
      }
      case CreatedEventVerb => {
        Log.info("New event received for %s".format(ids))
        if (Event.EDMLogisticsInventorySubject == ev.subject) {
          ids.foreach(titanTradeStoreManager.updateInventory(Environment(NullAtomicEnvironment(marketDay.startOfDay)), _))
        }
      }
      case CancelledEventVerb | RemovedEventVerb => {
        Log.info("Cancelled / deleted event received for %s".format(ids))
        if (Event.EDMLogisticsInventorySubject == ev.subject) {
          ids.foreach(titanTradeStoreManager.deleteInventory)
        }
      }
    }
  }

  /**
   * handler for market data snapshot events
   *
   * todo, has the call for this been removed, if so how are we now generating these events (or are we not)?
   */
  def marketDataSnapshotEventHander(ev: Event) = {
    log.info("handler: Got a snapshot event to process %s".format(ev.toString))

    /**
     * logic:
     *   take the most recent previous snapshot of the day and compare values using that snapshot and the new one, if different send events containing ids
     *   else if there is no previous snapshot for today then do nothing
     */
    val payloads = ev.content.body.payloads.filter(p => StarlingSnapshotIdPayload == p.payloadType)
    val ids = payloads.map(p => p.key.identifier)

    ev.subject match {
      case StarlingMarketDataSnapshotIDSubject => {
        ev.verb match {
          case CreatedEventVerb => {
            environmentProvider.snapshots().filter(_.observationDay == Day.today).sortWith(_.id > _.id) match {
              case newSnapshotId :: previousSnapshotId :: _ => {
                val previousEnv = environmentProvider.environment(previousSnapshotId, Day.today)
                val newEnv = environmentProvider.environment(newSnapshotId, Day.today)
                val changedTradeIDs = titanTradeStoreManager.titanTradeStore.getAllForwards.filter{
                  fwd =>
                    val originalValuation = fwd.costsAndIncomeQuotaValueBreakdown(previousEnv)
                    val newValuation = fwd.costsAndIncomeQuotaValueBreakdown(newEnv)
                    originalValuation != newValuation
                }.map(_.tradeID)

                log.info("Trades revalued for new snapshot %s, number of changed valuations %d".format(newSnapshotId, changedTradeIDs.size))

                if (changedTradeIDs != Nil)
                  rabbitPublishChangedValueEvents(changedTradeIDs, RefinedMetalTradeIdPayload)
              }
              case _ =>
            }
          }
          case UpdatedEventVerb => {
            log.info("Unhandled event for updated snapshot %s".format(ids))
          }
          case CancelledEventVerb | RemovedEventVerb => {
            log.info("Unhandled cancelled / deleted event received for %s".format(ids))
          }
        }
      }
    }
  }


/*

    if (todaysSnapshots.size > 1) {
      ev.subject match {
        case StarlingMarketDataSnapshotIDSubject => {
          ev.verb match {
            case CreatedEventVerb => {
              val previousSnapshotId = todaysSnapshots(1).id.toString
              log.info("New marketData snapshot event, revaluing received event using old snapshot %s new snapshot %s".format(previousSnapshotId, newSnapshotId))

              // just some extra logging until we understand potential bug around new snapshot ids not being found
              val oldSnapshots = environmentProvider.snapshots
              log.debug("old snapshots %s".format(oldSnapshots.mkString(", ")))
              environmentProvider.updateSnapshotCache()
              val newSnapshots = environmentProvider.getSnapshots()
              log.debug("new snapshots %s".format(newSnapshots.mkString(", ")))

              val missingSnapshotIds = ids.filter(id => !newSnapshots.contains(id))
              if (missingSnapshotIds.size > 0) {
                log.warn("New snapshot id event received for %s not found in the environment provider {}".format(missingSnapshotIds.mkString(", "), newSnapshots.mkString(", ")))
              }

              val previousEnv = environmentProvider.environment(previousSnapshotId, Day.today)
              val newEnv = environmentProvider.environment(newSnapshotId, Day.today)
              val changedTradeIDs = titanTradeStoreManager.titanTradeStore.getAllForwards.filter{
                fwd =>
                  val originalValuation = valuationServices.valueForward(previousEnv, fwd)
                  val newValuation = valuationServices.valueForward(newEnv, fwd)
                  originalValuation != newValuation
              }.map(_.tradeID)

              log.info("Trades revalued for new snapshot %s, number of changed valuations %d".format(newSnapshotId, changedTradeIDs.size))

              if (changedTradeIDs != Nil)
                rabbitPublishChangedValueEvents(changedTradeIDs, RefinedMetalTradeIdPayload)

              val originalInventoryValuations = valuationServices.valueInventoryAssignments(Nil, previousEnv, previousSnapshotId)
              val newInventoryValuations = valuationServices.valueInventoryAssignments(Nil, newEnv, newSnapshotId)
              val inventoryIds = newInventoryValuations.assignmentValuationResults.keys.toList
              val changedInventoryValueIDs = inventoryIds.filter(id => originalInventoryValuations.assignmentValuationResults(id) != newInventoryValuations.assignmentValuationResults(id))

              log.info("Inventory assignments revalued for new snapshot %s, number of changed valuations %d".format(newSnapshotId, changedInventoryValueIDs.size))

              if (changedInventoryValueIDs != Nil)
                rabbitPublishChangedValueEvents(changedInventoryValueIDs, RefinedMetalTradeIdPayload)
            }
            case UpdatedEventVerb => {
              log.info("Unhandled event for updated snapshot %s".format(ids))
            }
            case CancelledEventVerb | RemovedEventVerb => {
              log.info("Unhandled cancelled / deleted event received for %s".format(ids))
            }
          }
        }
      }
    }
  }
  */


  // publish the valuation updated event contaning payloads of the trade id's whose trade valuations have changed
  private val publishStarlingChangedValueEvents = publishChangedValueEvents(StarlingSource, StarlingValuationServiceSubject) _

  private def publishChangedValueEvents(source: String, subject: String)
                                       (eventPublisher: Publisher)(ids: List[String], payloadTypeParam: String = RefinedMetalTradeIdPayload): Unit = {

    val payloadDetails = ids.map(id => (payloadTypeParam, id))
    val payloads = createPayloads(source)(payloadDetails)
    val events = createEvents(source, subject)(UpdatedEventVerb, payloads)
    eventPublisher.publish(events)
  }

  private def publishCreatedValuationEvents(eventPublisher: Publisher)(newValuations: List[(String, Boolean)]) = {
    val newValuationPayloads: List[(String, String)] = newValuations.flatMap(e => (RefinedMetalTradeIdPayload, e._1) ::(StarlingNewValuationServiceStatusPayload, e._2.toString) :: Nil)
    val payloads = createStarlingPayloads(newValuationPayloads)
    val newValuationEvents = createValuationServiceEvents(CreatedEventVerb, payloads)
    eventPublisher.publish(newValuationEvents)
  }

  private val createValuationServiceEvents = createEvents(StarlingSource, StarlingValuationServiceSubject) _

  private def createEvents(source: String, subject: String)(verb: EventVerbEnum, payloads: List[Payload]): JSONArray = {
    val keyIdentifier = System.currentTimeMillis.toString
    val ev = EventFactory().createEvent(subject, verb, source, keyIdentifier, payloads)
    ||> { new JSONArray } { r => r.put(ev.toJson) }
  }

  private val createStarlingPayloads = createPayloads(StarlingSource) _

  private def createPayloads(source: String)(payloads: List[(String, String)]): List[Payload] = {
    payloads.map(p => EventPayloadFactory().createPayload(p._1, source, p._2))
  }
}
