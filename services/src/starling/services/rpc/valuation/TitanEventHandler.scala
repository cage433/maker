package starling.services.rpc.valuation

import com.trafigura.events.DemultiplexerClient
import starling.daterange.Day
import com.trafigura.shared.events._
import com.trafigura.edm.shared.types.TitanId
import starling.rmi.{ProcessedEventStatus, RabbitEventDatabase}
import starling.titan.{TitanTradeUpdateResult, TitanTradeStoreManager}
import starling.tradestore.TradeStore.StoreResults
import starling.utils.{Broadcaster, Log}
import starling.gui.api.{SnapshotIDLabel, RefinedMetalsValuationChanged}

/**
 * top level received event handler for Titan Rabbit events
 */
class TitanEventHandler(broadcaster:Broadcaster,
                        titanTradeStoreManager : TitanTradeStoreManager,
                        environmentProvider : EnvironmentProvider,
                        db : RabbitEventDatabase) extends DemultiplexerClient with Log {

  import Event._

  def publishedChangedValueEvents(observationDay:Day, snapshotID:SnapshotIDLabel, tradeIds:Set[String]) {
    broadcaster.broadcast(RefinedMetalsValuationChanged(observationDay, snapshotID, tradeIds))
  }

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
    log.info("handler: Got a trade event to process, ID %s event %s".format(ev.key.identifier, ev.toString))

    val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
    val tradeIds: List[String] = tradePayloads.map(p => p.key.identifier)
    val titanIds: List[TitanId] = tradeIds.map(id => TitanId(id))
    val eventId = ev.key.identifier
    log.info("Trade event %s received for ids { %s }".format(eventId, tradeIds.mkString(", ")))

    val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment

    ev.subject match {
      case TradeSubject => {
        ev.verb match {
          case UpdatedEventVerb => {
            val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier.equalsIgnoreCase("completed")).size > 0
            if (completed) {
              val results = tradeIds.map(id => titanTradeStoreManager.updateTrade(env, id, eventId))
              val changedIDs = results.flatMap(_.changedValueTitanTradeIds)
              if (changedIDs != Nil)
                publishedChangedValueEvents(env.marketDay.day, snapshotID, changedIDs.toSet)

              log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotID, changedIDs.size))
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
    log.info("handler: Got a logistics event to process,  ID %s event %s".format(ev.key.identifier, ev.toString))

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
        val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment

        val results = inventoryIds.map(id => titanTradeStoreManager.updateInventory(env, id, eventId))
        val changedForwardIDs = results.flatMap(_.changedValueTitanTradeIds)

        if (changedForwardIDs != Nil) {
          publishedChangedValueEvents(env.marketDay.day, snapshotID, changedForwardIDs.toSet)
        }

        log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotID, changedForwardIDs.size))
        Some(results)
      }
      case CreatedEventVerb => {
        Log.info("New event received for %s".format(inventoryIds))
        ev.subject match {
          case EDMLogisticsInventorySubject | EDMLogisticsSalesAssignmentSubject => {
            val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment
            val results = inventoryIds.map(id => titanTradeStoreManager.updateInventory(env, id, eventId))
            val changedForwardIDs = results.flatMap(_.changedValueTitanTradeIds)
            if (changedForwardIDs != Nil) {
              publishedChangedValueEvents(env.marketDay.day, snapshotID, changedForwardIDs.toSet)
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
            val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment
            Some(inventoryIds.map(inventoryID => titanTradeStoreManager.removeSalesAssignment(env, inventoryID, eventId)))
          }
          case _ => None
        }
      }
      case _ => None
    }
  }

  private def getID(payload : Payload) = payload.key.identifier
}
