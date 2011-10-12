package starling.services.rpc.valuation

import com.trafigura.events.DemultiplexerClient
import starling.utils.Log
import starling.daterange.Day
import com.trafigura.services.rabbit.Publisher
import com.trafigura.shared.events._
import org.codehaus.jettison.json.JSONArray
import com.trafigura.common.control.PipedControl._
import com.trafigura.edm.shared.types.TitanId
import starling.curves.{NullAtomicEnvironment, Environment}
import starling.services.rabbit.{EventPayloadFactory, EventFactory, TitanRabbitEventServices}
import starling.rmi.{ProcessedEventStatus, RabbitEventDatabase}
import starling.titan.{TitanTradeUpdateResult, TitanTradeStoreManager}
import starling.tradestore.TradeStore.StoreResults

/**
 * top level received event handler for Titan Rabbit events
 */
class TitanEventHandler(rabbitEventServices : TitanRabbitEventServices,
                        titanTradeStoreManager : TitanTradeStoreManager,
                        environmentProvider : EnvironmentProvider,
                        db : RabbitEventDatabase) extends DemultiplexerClient with Log {


  import Event._

  lazy val rabbitPublishChangedValueEvents = publishStarlingChangedValueEvents(rabbitEventServices.rabbitEventPublisher)
  lazy val rabbitPublishNewValuationEvents = publishCreatedValuationEvents(rabbitEventServices.rabbitEventPublisher) _

  /**
   * top level titan event handler
   */
  def handle(ev: Event) {
    log.debug("Received event " + ev)
    try {
      if (ev == null)
        log.error("Recieved a null event")
      else {

        db.saveEvent(ev)

        val result : Option[List[TitanTradeUpdateResult]] = if (TrademgmtSource == ev.source && (TradeSubject == ev.subject)) {
          tradeMgmtTradeEventHander(ev)
        }
        else if (LogisticsSource.equalsIgnoreCase(ev.source) && (EDMLogisticsSalesAssignmentSubject.equalsIgnoreCase(ev.subject) ||
            EDMLogisticsInventorySubject.equalsIgnoreCase(ev.subject) || LogisticsQuotaSubject.equalsIgnoreCase(ev.subject))) {
          logisticsInventoryEventHander(ev)
        }
        else if (StarlingSource.equalsIgnoreCase(ev.source) && StarlingMarketDataSnapshotIDSubject.equalsIgnoreCase(ev.subject)) {
          marketDataSnapshotEventHander(ev)
        }
        else None

        val tradeStoreResults = result.map(r => {
          StoreResults(
            r.map(_.tradeStoreResults.inserted).sum,
            r.map(_.tradeStoreResults.deleted).sum,
            r.map(_.tradeStoreResults.updated).sum, 0)
        })

        db.updateEvent(ev, ProcessedEventStatus, None, result.map(_.map(_.toString).mkString(", ")), tradeStoreResults)
      }
    }
    catch {
      case e => {
        log.error("Error while processing event\n" + e.getStackTrace.mkString("\n"))
        try {
          db.updateEvent(ev, ProcessedEventStatus, Some(e))
        }
        catch {
          case e2 => {
            log.error("Error while updating rabbit message failure status, event was %s and status %s, update exception was %s".format(ev.toString, e.toString, e2.toString))
          }
        }
        throw e
      }
    }
  }

  /**
   * handler for trademgmt trade events
   */
  def tradeMgmtTradeEventHander(ev: Event) : Option[List[TitanTradeUpdateResult]] = {
    log.info("handler: Got a trade event to process %s".format(ev.toString))

    val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
    val tradeIds: List[String] = tradePayloads.map(p => p.key.identifier)
    val titanIds: List[TitanId] = tradeIds.map(id => TitanId(id))
    val eventId = ev.key.identifier
    log.info("Trade event %s received for ids { %s }".format(eventId, tradeIds.mkString(", ")))

    val (snapshotIDString, env) = environmentProvider.recentRepresentativeEnvironment

    ev.subject match {
      case TradeSubject => {
        ev.verb match {
          case UpdatedEventVerb => {
            val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier.equalsIgnoreCase("completed")).size > 0
            if (completed) {
              val results = tradeIds.map(id => titanTradeStoreManager.updateTrade(env, id, eventId))
              val changedIDs = results.flatMap(_.changedValueTitanTradeIds)
              if (changedIDs != Nil)
                rabbitPublishChangedValueEvents(changedIDs, RefinedMetalTradeIdPayload)

              log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
              Some(results)
            }
            None
          }
          case CreatedEventVerb => None // not handled, everything is driven from updated/cancelled events
          case CancelledEventVerb | RemovedEventVerb => {
            Log.info("Cancelled / deleted event received for %s".format(titanIds))
            Some(tradeIds.map(id => titanTradeStoreManager.deleteTrade(id, eventId)))
          }
          case _ => None
        }
      }
    }
  }

  /**
   * handler for logistics assignment events
   */
  def logisticsInventoryEventHander(ev: Event) : Option[List[TitanTradeUpdateResult]] = {
    log.info("handler: Got a logistics event to process %s".format(ev.toString))

    // get inventory ids from inventory or sales assignment subjects (sales assignment is what is raised when a sales quota is fully allocated/deallocated)
    val payloads = ev.content.body.payloads
    val inventoryIds: List[String] = if (Event.EDMLogisticsInventorySubject.equalsIgnoreCase(ev.subject) || EDMLogisticsSalesAssignmentSubject.equalsIgnoreCase(ev.subject)) {
      payloads.filter(p => Event.EDMLogisticsInventoryIdPayload.equalsIgnoreCase(p.payloadType)).map(p => getID(p))
    }
    else Nil

    val eventId = ev.key.identifier
    log.info("Logistics event %s received for ids { %s }".format(eventId, inventoryIds.mkString(", ")))

    val marketDay = Day.today.previousWeekday

    ev.verb match {
      case UpdatedEventVerb => {
        val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday match {
          case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId, marketDay))
          case None => ("No Snapshot found", Environment(NullAtomicEnvironment(marketDay.startOfDay)))
        }

        val results = inventoryIds.map(id => titanTradeStoreManager.updateInventory(env, id, eventId))
        val changedForwardIDs = results.flatMap(_.changedValueTitanTradeIds)

        if (changedForwardIDs != Nil) {
          rabbitPublishChangedValueEvents(changedForwardIDs, RefinedMetalTitanIdPayload)
        }

        log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedForwardIDs.size))
        Some(results)
      }
      case CreatedEventVerb => {
        Log.info("New event received for %s".format(inventoryIds))
        ev.subject match {
          case EDMLogisticsInventorySubject | EDMLogisticsSalesAssignmentSubject => {
            val results = inventoryIds.map(id => titanTradeStoreManager.updateInventory(Environment(NullAtomicEnvironment(marketDay.startOfDay)), id, eventId))
            val changedForwardIDs = results.flatMap(_.changedValueTitanTradeIds)
            if (changedForwardIDs != Nil) {
              rabbitPublishChangedValueEvents(changedForwardIDs, RefinedMetalTitanIdPayload)
            }
            Some(results)
          }
          case _ => None
        }
      }
      case CancelledEventVerb | RemovedEventVerb => {
        Log.info("Cancelled / deleted event received for %s".format(inventoryIds))
        ev.subject match {
          case EDMLogisticsInventorySubject => {
            Some(inventoryIds.map(id => titanTradeStoreManager.deleteInventory(id, eventId)))
          }
          case EDMLogisticsSalesAssignmentSubject => {
            val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday match {
              case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId, marketDay))
              case None => ("No Snapshot found", Environment(NullAtomicEnvironment(marketDay.startOfDay)))
            }
            Some(inventoryIds.map(inventoryID => titanTradeStoreManager.removeSalesAssignment(env, inventoryID, eventId)))
          }
          case _ => None
        }
      }
      case _ => None
    }
  }

  /**
   * handler for market data snapshot events
   */
  def marketDataSnapshotEventHander(ev: Event) : Option[List[TitanTradeUpdateResult]] = {
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
    None
  }

  private def getID(payload : Payload) = payload.key.identifier

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
