package starling.titan

import starling.curves.Environment
import starling.daterange.Timestamp
import EDMConversions._
import starling.tradestore.TradeStore.StoreResults
import starling.utils.ImplicitConversions._
import com.trafigura.edm.logistics.inventory.{CancelledInventoryItemStatus, InventoryItem, LogisticsQuota}
import starling.utils.{Startable, Log}
import com.trafigura.edm.common.units.TitanId
import starling.instrument.{ErrorInstrument, Trade}
import starling.utils.{TimingInfo, Stopwatch, Startable, Log}
import com.trafigura.edm.logistics.inventory.{LogisticsInventoryResponse, CancelledInventoryItemStatus, InventoryItem, LogisticsQuota}
import scala.collection.mutable
import com.trafigura.edm.trademgmt.trades.{CompletedTradeState, PhysicalTrade => EDMPhysicalTrade}
import collection.immutable.{List, Map}



case class TitanServiceCache(
  private val edmTradeServices: TitanTradeMgmtServices,
  private val logisticsServices: TitanLogisticsServices,
  private val edmQuotaDetails: mutable.Set[TradeManagementQuotaDetails],
  private val edmInventoryItems: mutable.Map[String, LogisticsInventory],
  private val isQuotaFullyAllocated: mutable.Map[String, Boolean]

) extends Log with Startable {
  def externalServicesAreReady: Boolean = edmTradeServices.servicesReady && logisticsServices.servicesReady

  type InventoryID = String

  private val lock = new Object
//  override def start = initialiseCaches
//
  def rebuild{
    edmQuotaDetails.clear
    edmQuotaDetails ++= edmTradeServices.getAllCompletedPhysicalTrades().flatMap{tr => TradeManagementQuotaDetails.buildListOfQuotaDetails(edmTradeServices, tr, "Get All")}

    val allInventory: LogisticsInventoryResponse = logisticsServices.getAllInventory()

    edmInventoryItems.clear
    edmInventoryItems ++= allInventory.associatedInventory.filter(i => i.status != CancelledInventoryItemStatus).map{inv => inv.id -> LogisticsInventory(inv, "Get All")}
    isQuotaFullyAllocated.clear
    isQuotaFullyAllocated ++= allInventory.associatedQuota.map(q => q.quotaTitanId.toString -> q.fullyAllocated)

    quotaIDToInventory.clear
    edmInventoryItems.values.foreach(addInventoryToQuotaIDToInventoryMap)
  }

  private val quotaIDToInventory = scala.collection.mutable.Map[String, List[LogisticsInventory]]()

  private def addInventoryToQuotaIDToInventoryMap(inv : LogisticsInventory){
    inv.quotaIDs.foreach{
       qId =>
         quotaIDToInventory += (qId -> (inv :: quotaIDToInventory.getOrElse(qId, Nil)))
    }
  }
  private def removeInvFromQuotaIDToInventoryMap(inv : LogisticsInventory){
    inv.quotaIDs.foreach{
      quotaID =>
        quotaIDToInventory.update(quotaID, quotaIDToInventory.getOrElse(quotaID, Nil).filterNot(_ == inv))
    }
  }

  edmInventoryLeaves.foreach(addInventoryToQuotaIDToInventoryMap)

  private def edmInventoryLeaves : List[LogisticsInventory] = {
    val allInventoryIds = edmInventoryItems.keySet
    val parentIds = edmInventoryItems.values.flatMap{inv => inv.parentID}.toSet
    allInventoryIds.filterNot(parentIds).map(edmInventoryItems).toList
  }


  def removeInventory(inventoryID : String) {
    lock.synchronized {
      edmInventoryItems.get(inventoryID).foreach {
        inv =>
          edmInventoryItems -= inventoryID
          inv.saleAssignment.foreach {
            ass =>
              isQuotaFullyAllocated.retain {
                case (qID, _) => qID != ass.quotaID
              }
          }
          removeInvFromQuotaIDToInventoryMap(inv)
      }
    }
  }

  def updateInventory(inventoryID : String, eventID : String) {
    lock.synchronized{

      val logisticsResponse: LogisticsInventoryResponse = logisticsServices.getInventoryById(inventoryID.toInt)
      assert(logisticsResponse.associatedInventory.size == 1, "Expected a single piece of inventory")
      val newInventory = logisticsResponse.associatedInventory.head

      val newQuotas = logisticsResponse.associatedQuota
      assert(newQuotas.size >= 1 && newQuotas.size <= 2, "Expected one or two quotas")

      log.debug("inventory update, got inventory:\n %s, \n%s \nwith quotas \n%s\n".format(inventoryID, newInventory, newQuotas.map(q => (q.quotaName, q.fullyAllocated))))

      var inventory: LogisticsInventory = LogisticsInventory(newInventory, eventID)
      edmInventoryItems.get(inventoryID).foreach(removeInvFromQuotaIDToInventoryMap)
      edmInventoryItems += (newInventory.id -> inventory)
      isQuotaFullyAllocated ++= newQuotas.map(q => q.quotaTitanId.toString -> q.fullyAllocated)
      addInventoryToQuotaIDToInventoryMap(inventory)
    }
  }

  def removeTrade(titanTradeID : String) {
    lock.synchronized{
      val quotaIDs = edmQuotaDetails.filter(_.titanTradeID != titanTradeID).map(_.quotaID)
      quotaIDToInventory.retain{case (quotaID, _) => !quotaIDs.contains(quotaID)}
      edmQuotaDetails.retain(_.titanTradeID != titanTradeID)
    }
  }

  def updateTrade(titanTradeID : String, eventID : String): List[Trade] = {
    lock.synchronized {
      edmQuotaDetails.retain(_.titanTradeID != titanTradeID)
      val newTrade = edmTradeServices.getTrade(TitanId(titanTradeID))

      assert(newTrade.state == CompletedTradeState, "Unexpected state for trade " + newTrade.state + ", expected " + CompletedTradeState)
      var quotaDetails = TradeManagementQuotaDetails.buildListOfQuotaDetails(edmTradeServices, newTrade, eventID)
      edmQuotaDetails ++= quotaDetails
      quotaDetails.flatMap(buildTradesForQuota)
    }
  }

  private val forwardBuilder = new PhysicalMetalForwardBuilder(edmTradeServices)
  def buildTradesForQuota(qd : TradeManagementQuotaDetails) : List[Trade] = {
    val quotaID = qd.quotaID
    val invItems = quotaIDToInventory.getOrElse(quotaID, Nil)
    val fullyAllocated = isQuotaFullyAllocated.getOrElse(quotaID, false)
    forwardBuilder.build(qd, invItems, fullyAllocated)
  }

  def buildAllTrades : List[Trade] = {
    edmQuotaDetails.flatMap(buildTradesForQuota).toList
  }
}



/**
 * Manage the trade store trades via changes at trade, quota, inventory and assignment level
 */
case class TitanTradeStoreManager(cache : TitanServiceCache, titanTradeStore : TitanTradeStore, rabbitReadyFn: () => Unit) extends Log with Startable {
  def externalServicesAreReady: Boolean = cache.externalServicesAreReady


  override def start = cache.start

  rabbitReadyFn()
  def allStarlingTrades = titanTradeStore.allStarlingTrades()

  /**
   * Returns list of trade ids that have changed value
   */
  def updateInventory(env : Environment, inventoryID : String, eventID : String) : TitanTradeUpdateResult = {

    log.info(">>updateInventory inventoryID %s, eventID %s".format(inventoryID, eventID))
    val result = try {

      val existingFwds = titanTradeStore.getAllForwards()
      cache.updateInventory(inventoryID, eventID)
      val tradeStoreResult = updateTradeStore(eventID)
      val updatedFwds = titanTradeStore.getAllForwards()

      val changedTitanTradeIds = (existingFwds.keySet ++ updatedFwds.keySet).flatMap{
        titanTradeId =>
          (existingFwds.get(titanTradeId), updatedFwds.get(titanTradeId)) match {
            case (Some(Right(oldFwd)), Some(Right(newFwd))) if oldFwd.costsAndIncomeQuotaValueBreakdown(env) == newFwd.costsAndIncomeQuotaValueBreakdown(env) => None
            case _ => Some(titanTradeId)
          }
      }.toList

      TitanTradeUpdateResult(tradeStoreResult, changedTitanTradeIds, changedTitanTradeIds.size > 0)
    }
    catch {
      case ex => {
        Log.error("update trade failed ", ex)
        throw ex
      }
    }

    log.info("<<updateInventory inventoryID %s, eventID %s".format(inventoryID, eventID))
    result
  }

  def reimportAllTrades : StoreResults = {
    cache.rebuild
    updateTradeStore("Reimporting trades")
  }
  def updateTradeStore(eventID : String) : StoreResults = {
    log.info(">>updateTradeStore eventID %s".format(eventID))

    val sw = new Stopwatch()
    val updatedStarlingTrades = cache.buildAllTrades
    log.info("Got %d updated starling trades, took %s".format(updatedStarlingTrades.size, sw.toString))

    val duplicates = updatedStarlingTrades.groupBy(_.tradeID).filter(kv => kv._2.size > 1)

    if (!duplicates.isEmpty) Log.error("Duplicate trades found %s".format(duplicates.mkString(", ")))

    val filteredStarlingTrades = updatedStarlingTrades.filterNot(t => duplicates.keySet.contains(t.tradeID)).toList

    val duplicatedErrorStarlingTrades = duplicates.values.map(trades => {
      val trade = trades.head
      Trade(trade.tradeID,
        TitanTradeAttributes.dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(trade.titanTradeID.getOrElse("No Titan Trade Id available"), List(eventID)),
        ErrorInstrument(new Exception("Duplicate trade id found")))
    }).toList

    val result = titanTradeStore.storeTrades(
      {trade : Trade => true},
      filteredStarlingTrades ::: duplicatedErrorStarlingTrades,
      new Timestamp
    )

    log.info("<<updateTradeStore eventID %s".format(eventID))

    result
  }

  /**
   * update / create a new trade, returns true if update changes valuation
   */
  def updateTrade(env : Environment, titanTradeID : String, eventID : String) : TitanTradeUpdateResult = {

    log.info(">>updateTrade titanTradeID %s, eventID %s".format(titanTradeID, eventID))
    val result = try {

      val existingFwd = titanTradeStore.getForward(titanTradeID)

      val newStarlingTrades : List[Trade] = cache.updateTrade(titanTradeID, eventID)

      val tradeStoreResult = titanTradeStore.storeTrades(
        {trade : Trade => trade.titanTradeID == Some(titanTradeID)},
        newStarlingTrades,
        new Timestamp
      )

      val updatedFwd = titanTradeStore.getForward(titanTradeID)

      val valueChanged = (existingFwd, updatedFwd) match {
        case (Right(oldFwd), Right(newFwd)) => oldFwd.costsAndIncomeQuotaValueBreakdown(env) != newFwd.costsAndIncomeQuotaValueBreakdown(env)
        case (Left(_), Left(_)) => false
        case _ => true
      }

      TitanTradeUpdateResult(tradeStoreResult, if (valueChanged) List(titanTradeID) else Nil, valueChanged)
    }
    catch {
      case ex => {
        Log.error("update trade failed ", ex)
        throw ex
      }
    }
    log.info("<<updateTrade titanTradeID %s, eventID %s".format(titanTradeID, eventID))
    result
  }

  def deleteTrade(titanTradeID : String, eventID : String) : TitanTradeUpdateResult = {
    log.info(">>deleteTrade titanTradeID %s, eventID %s".format(titanTradeID, eventID))
    cache.removeTrade(titanTradeID)
    val result = TitanTradeUpdateResult(
      titanTradeStore.storeTrades(
        {trade => trade.titanTradeID == Some(titanTradeID)},
        Nil,
        new Timestamp
      ), Nil, false)
    log.info("<<deleteTrade titanTradeID %s, eventID %s".format(titanTradeID, eventID))
    result
  }

  def deleteInventory(titanInventoryID : String, eventID : String) : TitanTradeUpdateResult = {
    log.info(">>deleteInventory titanInventoryID %s, eventID %s".format(titanInventoryID, eventID))

    // since we may have to adjust the unallocated sales portion of a trade quota we
    // rebuild the whole trade store (this can probably be optimised but it's likely to be tricky/error prone)
    cache.removeInventory(titanInventoryID)
    val tradeStoreResult = updateTradeStore(eventID)
    /*
    val result = TitanTradeUpdateResult(
      titanTradeStore.storeTrades(
        {trade => trade.titanInventoryID == Some(titanInventoryID)},
        Nil,
        new Timestamp
      ), Nil, false)
    */
    log.info("<<deleteInventory titanInventoryID %s, eventID %s".format(titanInventoryID, eventID))
    TitanTradeUpdateResult(tradeStoreResult, Nil, false)
  }

  /**
   * removal of a sales assignment only, we need to remove and refresh the inventory in the trade store
   * removal of inventory from cache will purge associated logistics quota and it will get updated
   * during the inventory update
   */
  def removeSalesAssignment(env : Environment, titanInventoryID : String, eventID : String) : TitanTradeUpdateResult = {
    log.info(">>removeSalesAssignment titanInventoryID %s, eventID %s".format(titanInventoryID, eventID))
    cache.removeInventory(titanInventoryID)
    val result = updateInventory(env, titanInventoryID, eventID)
    log.info("<<removeSalesAssignment titanInventoryID %s, eventID %s".format(titanInventoryID, eventID))
    result
  }
}

case class TitanTradeUpdateResult(
      tradeStoreResults : StoreResults,
      changedValueTitanTradeIds : List[String],
      hasValue : Boolean) {

  override def toString : String =
    "tradeStoreResults {%s}, changedValueTitanTradeIds {%s}, hasValue {%s}".format(tradeStoreResults.toString, changedValueTitanTradeIds.mkString(", "), hasValue)
}
