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
import collection.immutable.Map

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
        if (TrademgmtSource == ev.source && (TradeSubject == ev.subject )) {
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
    // manager.handleEvent
    // manaeger.updateAllTrades
    log.info("handler: Got a trade event to process %s".format(ev.toString))

    val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
    val tradeIds: List[String] = tradePayloads.map(p => p.key.identifier)
    val titanIds: List[TitanId] = tradeIds.map(id => TitanId(id))
    log.info("Trade event received for ids { %s }".format(tradeIds.mkString(", ")))
    val (snapshotIDString, env) = environmentProvider.recentRepresentativeEnvironment
    ev.subject match {
      case TradeSubject => {
        ev.verb match {
          case UpdatedEventVerb => {
            val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier.equalsIgnoreCase("completed")).size > 0
            if (completed) {
              val changedIDs = tradeIds.filter(titanTradeStoreManager.updateTrade(env, _))
              if (changedIDs != Nil)
                rabbitPublishChangedValueEvents(changedIDs, RefinedMetalTradeIdPayload)

              log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
            }
          }
          case CreatedEventVerb =>
          case CancelledEventVerb | RemovedEventVerb => {
            Log.info("Cancelled / deleted event received for %s".format(titanIds))
            tradeIds.foreach(titanTradeStoreManager.deleteTrade)
          }
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

    // get inventory ids from inventory or sales assignment subjects (sales assignment is what is raised when a sales quota is fully allocated/deallocated)
    val payloads = ev.content.body.payloads
    val inventoryIds: List[String] = if (Event.EDMLogisticsInventorySubject.equalsIgnoreCase(ev.subject) || EDMLogisticsSalesAssignmentSubject.equalsIgnoreCase(ev.subject)) {
      payloads.filter(p => Event.EDMLogisticsInventoryIdPayload.equalsIgnoreCase(p.payloadType)).map(p => getID(p))
    }
    else Nil

    log.info("Logistics event received for ids { %s }".format(inventoryIds.mkString(", ")))

    val marketDay = Day.today.previousWeekday

    ev.verb match {
      case UpdatedEventVerb => {
        val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday match {
          case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId, marketDay))
          case None => ("No Snapshot found", Environment(NullAtomicEnvironment(marketDay.startOfDay)))
        }

        val changedForwardIDs = inventoryIds.flatMap(id => titanTradeStoreManager.updateInventory(env, id))

        if (changedForwardIDs != Nil) {
          rabbitPublishChangedValueEvents(changedForwardIDs, RefinedMetalTitanIdPayload)
        }

        log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedForwardIDs.size))
      }
      case CreatedEventVerb => {
        Log.info("New event received for %s".format(inventoryIds))
        if (Event.EDMLogisticsInventorySubject == ev.subject || EDMLogisticsSalesAssignmentSubject == ev.subject) {
          val changedForwardIDs = inventoryIds.flatMap(titanTradeStoreManager.updateInventory(Environment(NullAtomicEnvironment(marketDay.startOfDay)), _))
          if (changedForwardIDs != Nil) {
            rabbitPublishChangedValueEvents(changedForwardIDs, RefinedMetalTitanIdPayload)
          }
        }
      }
      case CancelledEventVerb | RemovedEventVerb => {
        Log.info("Cancelled / deleted event received for %s".format(inventoryIds))
        if (Event.EDMLogisticsInventorySubject == ev.subject || EDMLogisticsSalesAssignmentSubject == ev.subject) {
          inventoryIds.foreach(titanTradeStoreManager.deleteInventory)
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
                val changedTradeIDs = titanTradeStoreManager.titanTradeStore.getAllForwards.collect{
                  case (_, Right(fwd)) => fwd
                }.filter{
                  fwd =>
                    val originalValuation = fwd.costsAndIncomeQuotaValueBreakdown(previousEnv)
                    val newValuation = fwd.costsAndIncomeQuotaValueBreakdown(newEnv)
                    originalValuation != newValuation
                }.map(_.titanTradeID).toList

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
