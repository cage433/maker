package starling.services.rpc.valuation

import com.trafigura.events.DemultiplexerClient
import starling.daterange.Day
import com.trafigura.shared.events._
import starling.rmi.{ProcessedEventStatus, RabbitEventDatabase}
import starling.titan.{TitanTradeUpdateResult, TitanTradeStoreManager}
import starling.tradestore.TradeStore.StoreResults
import starling.utils.{Log}
import starling.gui.api.{SnapshotIDLabel, RefinedMetalsValuationChanged}
import com.trafigura.edm.common.units.TitanId
import starling.curves.Environment
import starling.db.SnapshotID
import starling.manager.Broadcaster

/**
 * top level received event handler for Titan Rabbit events
 */
class TitanEventHandler(broadcaster:Broadcaster,
                        titanTradeStoreManager : TitanTradeStoreManager,
                        environmentProvider : EnvironmentProvider,
                        db : RabbitEventDatabase) extends DemultiplexerClient with Log {

  import Event._

  def publishedChangedValueEvents(observationDay:Day, snapshotID:SnapshotIDLabel, tradeId:String) {
    broadcaster.broadcast(RefinedMetalsValuationChanged(observationDay, snapshotID, tradeId))
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
   * C&I have a bug whereby they only picked up the first trade id. No idea now why we published a 
   * list of trade ids. They'll fix eventually but for now we'll publish seperate events
   */
  private def publishManyChangedValueEvents(env : Environment, snapshotID : SnapshotIDLabel, changedIDs : List[String]){
    changedIDs.distinct.foreach{
      id => 
        publishedChangedValueEvents(env.marketDay.day, snapshotID, id)
    }
  }
  /**
   * handler for trademgmt trade events
   */
  def publishOnChangedValues(results: Option[List[TitanTradeUpdateResult]], snapshotID: SnapshotIDLabel, env: Environment) {
    results match {
      case Some(listOfResults) =>
        val changedForwardIDs: List[String] = listOfResults.flatMap(_.changedValueTitanTradeIds)
        log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotID, changedForwardIDs.size))
        publishManyChangedValueEvents(env, snapshotID, changedForwardIDs)
      case None =>
    }
  }

  def tradeMgmtTradeEventHander(ev: Event) : Option[List[TitanTradeUpdateResult]] = {
    log.info("handler: Got a trade event to process, ID %s event %s".format(ev.key.identifier, ev.toString))

    val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTitanIdPayload == p.payloadType)
    val tradeIds: List[String] = tradePayloads.map(p => p.key.identifier)
    val titanIds: List[TitanId] = tradeIds.map(id => TitanId(id))
    val eventId = ev.key.identifier
    log.info("Trade event %s received for ids { %s }".format(eventId, tradeIds.mkString(", ")))

    val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment

    val results = ev.subject match {
      case TradeSubject => {
        ev.verb match {
          case UpdatedEventVerb => {
            val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier.equalsIgnoreCase("completed")).size > 0
            if (completed) {
              val results = tradeIds.map(id => titanTradeStoreManager.updateTrade(env, id, eventId))
              val changedIDs = results.flatMap(_.changedValueTitanTradeIds)

              log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotID, changedIDs.size))
              Some(results)
            }
            else None
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

    publishOnChangedValues(results, snapshotID, env)

    results
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
    lazy val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment

    val results: Option[List[TitanTradeUpdateResult]] = ev.verb match {
      case UpdatedEventVerb => {
        val results: List[TitanTradeUpdateResult] = inventoryIds.map(id => titanTradeStoreManager.updateInventory(env, id, eventId))
        Some(results)
      }
      case CreatedEventVerb => {
        Log.info("New event received for %s".format(inventoryIds))
        ev.subject match {
          case EDMLogisticsInventorySubject | EDMLogisticsSalesAssignmentSubject => {
            val results = inventoryIds.map(id => titanTradeStoreManager.updateInventory(env, id, eventId))
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
    publishOnChangedValues(results, snapshotID, env)
    results
  }

  private def getID(payload : Payload) = payload.key.identifier
}
