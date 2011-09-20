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
import starling.titan.{TitanTacticalRefData, TitanTradeCache}


/**
 * handler for Titan rabbit events
 */
class TitanEventHandler(rabbitEventServices : TitanRabbitEventServices,
                   valuationServices : ValuationService,
                   titanTradeCache : TitanTradeCache,
                   titanInventoryCache : TitanLogisticsInventoryCache,
                   environmentProvider : EnvironmentProvider,
                   refData : TitanTacticalRefData,
                   db : RabbitEventDatabase) extends DemultiplexerClient with Log {

  import Event._

  lazy val rabbitPublishChangedValueEvents = publishStarlingChangedValueEvents(rabbitEventServices.rabbitEventPublisher)
  lazy val rabbitPublishNewValuationEvents = publishCreatedValuationEvents(rabbitEventServices.rabbitEventPublisher) _

  /**
   * top level titan event handler
   */
  def handle(ev: Event) {
    if (ev == null) log.warn("Got a null event")
    else {
      if (TrademgmtSource == ev.source && (TradeSubject == ev.subject || NeptuneTradeSubject == ev.subject)) {
        // Must be some form of trade event from trademgmt source
        tradeMgmtTradeEventHander(ev)
      }
      else if (LogisticsSource.equalsIgnoreCase(ev.source) && (EDMLogisticsSalesAssignmentSubject.equalsIgnoreCase(ev.subject) || EDMLogisticsInventorySubject.equalsIgnoreCase(ev.subject))) {
        logisticsAssignmentEventHander(ev)
      }
      db.saveEvent(ev)
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

    ev.subject match {
      case TradeSubject => {
        ev.verb match {
          case UpdatedEventVerb => {
            val (snapshotIDString, env) = getSnapshotAndEnv
            val changedIDs = tradeIds.filter{
              id =>
                val originalValue = valuationServices.valueTradeQuotas(id, env, snapshotIDString)
                titanTradeCache.removeTrade(TitanId(id))
                titanTradeCache.addTrade(TitanId(id))
                val currentValue = valuationServices.valueTradeQuotas(id, env, snapshotIDString)
                originalValue != currentValue
            }
            if (changedIDs != Nil)
              rabbitPublishChangedValueEvents(changedIDs, RefinedMetalTradeIdPayload)

            log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
          }
          case CreatedEventVerb => {
            Log.info("New event received for %s".format(tradeIds))
            titanIds.foreach(titanTradeCache.addTrade)
          }
          case CancelledEventVerb | RemovedEventVerb => {
            Log.info("Cancelled / deleted event received for %s".format(titanIds))
            titanIds.foreach(titanTradeCache.removeTrade)
          }
        }
      }
      // handle publishing of valuation status events (events saying if a new (completed) trade can be valued successfully or not)
      case NeptuneTradeSubject => {
        val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier == "completed").size > 0
        if (completed) {
          val (snapshotIDString, env) = getSnapshotAndEnv
          val valuationResults : List[(String, Boolean)] = tradeIds.map{
            id => valuationServices.valueTradeQuotas(id, env, snapshotIDString) match {
              case (_, Right(_)) => (id, true)
              case (_, Left(_)) => (id, false)
            }
          }
          rabbitPublishNewValuationEvents(valuationResults)
        }
      }
    }
  }

  /**
   * handler for logistics assignment events
   */
  def logisticsAssignmentEventHander(ev: Event) = {
    log.info("handler: Got an assignment event to process %s".format(ev.toString))

    val payloads = ev.content.body.payloads
    val ids: List[String] = if (Event.EDMLogisticsSalesAssignmentSubject == ev.subject) {
      payloads.map(p => titanInventoryCache.inventoryIDFromAssignmentID(p.key.identifier)) // map back to inventory id
    }
    else if (Event.EDMLogisticsInventorySubject == ev.subject) {
      payloads.map(p => p.key.identifier)
    }
    else Nil

    log.info("Assignment event received for ids { %s }".format(ids.mkString(", ")))

//    def tradesForInventory(inventoryID: String): List[Trade] = {
//      val tradeConverter = TradeConverter(refData, titanTradeCache)
//      titanInventoryCache.getAssignmentsForInventory(inventoryID) map (tradeConverter.toTrade)
//    }

//    def writeToTradeStore(inventoryID: String) {
//      titanTradeStore match {
//        case Some(tradeStore) => {
//          val trades: Seq[Trade] = tradesForInventory(inventoryID)
//          val tradeIDs = trades.map(_.tradeID)
//          tradeStore.storeTrades((trade) => tradeIDs.contains(trade.tradeID), trades, new Timestamp())
//        }
//        case None =>
//      }
//    }
//    def deleteFromTradeStore(inventoryID: String) {
//      titanTradeStore match {
//        case Some(tradeStore) => {
//          val tradeIDs = tradesForInventory(inventoryID).map(_.tradeID)
//          tradeStore.storeTrades((trade) => tradeIDs.contains(trade.tradeID), Nil, new Timestamp())
//        }
//        case None =>
//      }
//    }

    ev.verb match {
      case UpdatedEventVerb => {
        val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday() match {
          case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId))
          case None => ("No Snapshot found", Environment(NullAtomicEnvironment((Day.today - 1).startOfDay)))
        }

        val originalInventoryAssignmentValuations = valuationServices.valueInventoryAssignments(ids, env, snapshotIDString)
        ids.foreach {
          id => titanInventoryCache.removeInventory(id); titanInventoryCache.addInventory(id)
        }
        //ids.foreach(writeToTradeStore(_))
        val newInventoryAssignmentValuations = valuationServices.valueInventoryAssignments(ids, env, snapshotIDString)

        val changedIDs = ids.filter {
          id => newInventoryAssignmentValuations.assignmentValuationResults(id) != originalInventoryAssignmentValuations.assignmentValuationResults(id)
        }

        if (changedIDs != Nil) {
          rabbitPublishChangedValueEvents(changedIDs, EDMLogisticsInventoryIdPayload)
        }

        log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
      }
      case CreatedEventVerb => {
        Log.info("New event received for %s".format(ids))
        if (Event.EDMLogisticsInventorySubject == ev.subject) {
          ids.foreach(titanInventoryCache.addInventory)
          //ids.foreach(writeToTradeStore)
        }
      }
      case CancelledEventVerb | RemovedEventVerb => {
        Log.info("Cancelled / deleted event received for %s".format(ids))
        if (Event.EDMLogisticsInventorySubject == ev.subject) {
          //ids.foreach(deleteFromTradeStore)
          ids.foreach(titanInventoryCache.removeInventory)
        }
      }
    }
  }

  /**
   * handler for market data snapshot events
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
    val newSnapshotId = payloads.map(p => p.key.identifier).max
    val todaysSnapshots: List[SnapshotID] = environmentProvider.snapshotIDs(Some(Day.today)).sortWith(_.id > _.id)
    log.info("Snapshot event received for ids { %s }, using '%s'".format(ids.mkString(", "), newSnapshotId))

    if (todaysSnapshots.size > 1) {
      ev.subject match {
        case StarlingMarketDataSnapshotIDSubject => {
          ev.verb match {
            case CreatedEventVerb => {
              val previousSnapshotId = todaysSnapshots(1).id.toString
              log.info("New marketData snapshot event, revaluing received event using old snapshot %s new snapshot %s".format(previousSnapshotId, newSnapshotId))
              val previousEnv = environmentProvider.environment(previousSnapshotId)
              val newEnv = environmentProvider.environment(newSnapshotId)
              val tradeIds = titanTradeCache.getAllTrades().map{trade => trade.titanId.value}.toList
              val originalTradeValuations = tradeIds.map{id => id -> valuationServices.valueTradeQuotas(id, previousEnv, previousSnapshotId)._2}.toMap
              val newTradeValuations = tradeIds.map{id => id -> valuationServices.valueTradeQuotas(id, newEnv, newSnapshotId)._2}.toMap
              val changedTradeIDs = tradeIds.filter(id => newTradeValuations(id) != originalTradeValuations(id))

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

  def getSnapshotAndEnv: (String, Environment) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday() match {
    case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId))
    case None => ("No Snapshot found", Environment(NullAtomicEnvironment((Day.today - 1).startOfDay)))
  }

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
