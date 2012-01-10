package starling.titan

import starling.curves.Environment
import starling.daterange.Timestamp
import EDMConversions._
import collection.immutable.Map
import com.trafigura.edm.trademgmt.trades.{CompletedTradeState, PhysicalTrade => EDMPhysicalTrade}
import starling.tradestore.TradeStore.StoreResults
import starling.utils.ImplicitConversions._
import com.trafigura.edm.logistics.inventory.{CancelledInventoryItemStatus, InventoryItem, LogisticsQuota}
import com.trafigura.edm.common.units.TitanId
import starling.utils.{Stopwatch, Startable, Log}
import starling.instrument.{ErrorInstrument, Trade}

case class TitanServiceCache(private val refData : TitanTacticalRefData,
                             private val edmTradeServices : TitanEdmTradeService,
                             private val logisticsServices : TitanLogisticsServices) extends Log with Startable {
  import scala.collection.mutable
  type InventoryID = String
  val edmTrades = mutable.Set[EDMPhysicalTrade]()
  val edmInventoryItems = mutable.Map[InventoryID, InventoryItem]()
  val edmLogisticsQuotas = scala.collection.mutable.Set[LogisticsQuota]()

  override def start = initialiseCaches

  def initialiseCaches{
    edmTrades.clear
    edmTrades ++= edmTradeServices.getAllCompletedPhysicalTrades()

    val allInventory = {
      val sw = new Stopwatch()
      val inventory = logisticsServices.inventoryService.service.getAllInventory()
      log.info("Loaded %d inventory in %s".format(inventory.associatedInventory.size, sw.toString))
      inventory
    }

    edmInventoryItems.clear
    edmInventoryItems ++= allInventory.associatedInventory.filter(i => i.status != CancelledInventoryItemStatus).map{inv => inv.id -> inv}
    edmLogisticsQuotas.clear
    edmLogisticsQuotas ++= allInventory.associatedQuota
  }

  def tradeForwardBuilder: PhysicalMetalForwardBuilder = {
    val inventoryByQuotaID : Map[TitanId, Traversable[InventoryItem]] = edmInventoryLeaves.flatMap{
      inv : InventoryItem => 
        val quotaNames = inv.purchaseAssignment.quotaTitanId :: Option(inv.salesAssignment).map(_.quotaTitanId).toList
        quotaNames.map(_ → inv)
    }.groupValues
    val fullyAllocatedQuotaMap = edmLogisticsQuotas.map(q => q.quotaTitanId -> q.fullyAllocated).toMap
    new PhysicalMetalForwardBuilder(refData, inventoryByQuotaID, fullyAllocatedQuotaMap)
  }
  def removeTrade(titanTradeID : String) {
    edmTrades.retain(_.identifier.value != titanTradeID)
  }
  def edmInventoryLeaves : List[InventoryItem] = {
    val allInventoryIds = edmInventoryItems.keySet
    val parentIds = edmInventoryItems.values.flatMap{inv => inv.parentId.map(_.toString)}.toSet
    allInventoryIds.filterNot(parentIds).map(edmInventoryItems).toList
  }

  def removeInventory(inventoryID : String) {
    val salesQuota = edmInventoryItems.get(inventoryID) match {
      case Some(inv) => {
        edmInventoryItems -= inventoryID
        Option(inv.salesAssignment)
      }
      case None => None
    }

    // remove the associated sales quota, it will be repopulated next time it's allocated
    salesQuota match {
      case Some(lsq) => edmLogisticsQuotas.retain(lq => lq.quotaName  != lsq.quotaName)
      case None =>
    }
  }

  def updateTrade(titanTradeID : String) = {
    edmTrades --= edmTrades.filter(_.identifier.value == titanTradeID)
    val newTrade = edmTradeServices.getTrade(TitanId(titanTradeID))

    log.info("trade state = " + newTrade.state)
    if (newTrade.state != CompletedTradeState) {
      log.error("Wrong state for trade " + newTrade.state)
    }

    edmTrades += newTrade
    newTrade
  }

  def updateInventory(inventoryID : String) {
    val logisticsResponse = logisticsServices.inventoryService.service.getInventoryById(inventoryID.toInt)
    assert(logisticsResponse.associatedInventory.size == 1, "Expected a single piece of inventory")
    val newInventory = logisticsResponse.associatedInventory.head

    val newQuotas = logisticsResponse.associatedQuota
    assert(newQuotas.size >= 1 && newQuotas.size <= 2, "Expected one or two quotas")

    log.info("inventory update, got inventory:\n %s, \n%s \nwith quotas \n%s\n".format(inventoryID, newInventory, newQuotas.map(q => (q.quotaName, q.fullyAllocated))))

    edmInventoryItems += (newInventory.id -> newInventory)
    edmLogisticsQuotas.retain(lq => !newQuotas.map(_.quotaName).contains(lq.quotaName))
    edmLogisticsQuotas ++= newQuotas
  }
}

/**
 * Manage the trade store trades via changes at trade, quota, inventory and assignment level
 */
case class TitanTradeStoreManager(cache : TitanServiceCache, titanTradeStore : TitanTradeStore) extends Log with Startable {

  override def start = cache.start

  def allStarlingTrades = titanTradeStore.allStarlingTrades()

  def edmInventoryItems_ = Map() ++ cache.edmInventoryItems
  def edmLogisticsQuotas_ = Set() ++ cache.edmLogisticsQuotas

  /**
   * Returns list of trade ids that have changed value
   */
  def updateInventory(env : Environment, inventoryID : String, eventID : String) : TitanTradeUpdateResult = {

    log.info(">>updateInventory inventoryID %s, eventID %s".format(inventoryID, eventID))
    val result = try {

      val existingFwds = titanTradeStore.getAllForwards()
      cache.updateInventory(inventoryID)
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

  def updateTradeStore(eventID : String) : StoreResults = {
    log.info(">>updateTradeStore eventID %s".format(eventID))

    val sw = new Stopwatch()
    val updatedStarlingTrades = cache.edmTrades.flatMap(trade => cache.tradeForwardBuilder.apply(trade, eventID))

    log.info("Got %d updated starling trades, took %s".format(updatedStarlingTrades.size, sw.toString))

    val duplicates = updatedStarlingTrades.groupBy(_.tradeID).filter(kv => kv._2.size > 1)

    if (!duplicates.isEmpty) Log.error("Duplicate trades found %s".format(duplicates.mkString(", ")))

    val filteredStarlingTrades = updatedStarlingTrades.filterNot(t => duplicates.keySet.contains(t.tradeID)).toList

    val duplicatedErrorStarlingTrades = duplicates.values.map(trades => {
      val trade = trades.head
      Trade(trade.tradeID,
        TitanTradeAttributes.dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(trade.titanTradeID.getOrElse("No Titan Trade Id available"), eventID),
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
      val newEDMTrade = cache.updateTrade(titanTradeID)

      val newStarlingTrades : List[Trade] = cache.tradeForwardBuilder(newEDMTrade, eventID)

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

object TitanTradeStoreManager{
  def apply(refData : TitanTacticalRefData, titanTradeStore : TitanTradeStore,
            edmTradeServices : TitanEdmTradeService, logisticsServices : TitanLogisticsServices) : TitanTradeStoreManager = TitanTradeStoreManager(TitanServiceCache(refData, edmTradeServices, logisticsServices), titanTradeStore)
}

case class TitanTradeUpdateResult(
      tradeStoreResults : StoreResults,
      changedValueTitanTradeIds : List[String],
      hasValue : Boolean) {

  override def toString : String =
    "tradeStoreResults {%s}, changedValueTitanTradeIds {%s}, hasValue {%s}".format(tradeStoreResults.toString, changedValueTitanTradeIds.mkString(", "), hasValue)
}
