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

  def publishedChangedValueEvents(tradeId:String) {
    broadcaster.broadcast(RefinedMetalsValuationChanged(tradeId))
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
  private def publishManyChangedValueEvents(changedIDs : List[String]){
    changedIDs.distinct.foreach{
      id => 
        publishedChangedValueEvents(id)
    }
  }
  /**
   * handler for trademgmt trade events
   */
  def publishOnChangedValues(results: Option[List[TitanTradeUpdateResult]]) {
    results match {
      case Some(listOfResults) =>
        val changedForwardIDs: List[String] = listOfResults.flatMap(_.changedValueTitanTradeIds)
        log.info("Assignments revalued for received event, number of changed valuations %d".format(changedForwardIDs.size))
        publishManyChangedValueEvents(changedForwardIDs)
      case None =>
    }
  }

  def tradeMgmtTradeEventHander(ev: Event) : Option[List[TitanTradeUpdateResult]] = {
    log.info("handler: Got a trade event to process, ID %s event %s".format(ev.key.identifier, ev.toString))

    val tradePayloads = ev.content.body.payloads.filter(p => RefinedMetalTitanIdPayload == p.payloadType)
    val tradeIds: List[String] = tradePayloads.map(p => p.key.identifier)
    val eventId = ev.key.identifier
    log.info("Trade event %s received for ids { %s }".format(eventId, tradeIds.mkString(", ")))


    val results = (ev.subject, ev.verb) match {
      case (TradeSubject, UpdatedEventVerb) => {
        ev.content.body.payloads.find(p => TradeStatusPayload == p.payloadType && p.key.identifier.equalsIgnoreCase("completed")).map{
          _ =>
          val (snapshotID, env) = environmentProvider.lastValuationSnapshotEnvironment
          val results = tradeIds.map(id => titanTradeStoreManager.updateTrade(env, id, eventId))
          val changedIDs = results.flatMap(_.changedValueTitanTradeIds)

          log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotID, changedIDs.size))
          results
        }
      }
      case (TradeSubject, CancelledEventVerb | RemovedEventVerb) => {
        Log.info("Cancelled / deleted event received for %s".format(tradeIds))
        Some(tradeIds.map(id => titanTradeStoreManager.deleteTrade(id, eventId)))
      }
      case _ => None
    }

    publishOnChangedValues(results)

    results
  }

  /**
   * handler for logistics assignment events
   */
  def logisticsInventoryEventHander(ev: Event) : Option[List[TitanTradeUpdateResult]] = {
    val eventId = ev.key.identifier

    // get inventory ids from inventory or sales assignment subjects (sales assignment is what is raised when a sales quota is fully allocated/deallocated)
    val inventoryIds: List[String] = ev.subject match {
      case EDMLogisticsInventorySubject | EDMLogisticsSalesAssignmentSubject => {
        val payloads = ev.content.body.payloads
        payloads.filter(p => EDMLogisticsInventoryIdPayload.equalsIgnoreCase(p.payloadType)).map(_.key.identifier)
      }
      case _ => Nil
    }

    log.info("Logistics event %s received for ids { %s }".format(eventId, inventoryIds.mkString(", ")))

    lazy val (_, env) = environmentProvider.lastValuationSnapshotEnvironment

    val results: Option[List[TitanTradeUpdateResult]] = (ev.verb, ev.subject) match {
      case (UpdatedEventVerb | CreatedEventVerb, EDMLogisticsInventorySubject | EDMLogisticsSalesAssignmentSubject) => {
        val results: List[TitanTradeUpdateResult] = inventoryIds.map(id => titanTradeStoreManager.updateInventory(env, id, eventId))
        Some(results)
      }
      case (CancelledEventVerb | RemovedEventVerb, EDMLogisticsInventorySubject) => {
        Some(inventoryIds.map(id => titanTradeStoreManager.deleteInventory(id, eventId)))
      }
      case (CancelledEventVerb | RemovedEventVerb, EDMLogisticsSalesAssignmentSubject) => {
        Some(inventoryIds.map(inventoryID => titanTradeStoreManager.removeSalesAssignment(env, inventoryID, eventId)))
      }
      case _ => None
    }
    publishOnChangedValues(results)
    results
  }
}
