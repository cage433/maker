package starling.titan

import starling.curves.Environment
import com.trafigura.edm.shared.types.TitanId
import com.trafigura.edm.logistics.inventory.{EDMInventoryItem, EDMLogisticsQuota}
import starling.instrument.Trade
import starling.daterange.Timestamp
import starling.utils.Log
import EDMConversions._
import collection.immutable.Map
import com.trafigura.edm.trades.{CompletedTradeState, PhysicalTrade => EDMPhysicalTrade}
import starling.tradestore.TradeStore.StoreResults


trait SimpleCache[K, V] {
  val name : String
  def flush() : Unit
  def getAll() : Set[V]
  def get(id : K) : V
}

class DefaultSimpleCache[K, V]( val name : String,
                                idFn : V => K,
                                getAllFn : () => Set[V],
                                getFn : K => V) extends SimpleCache[K, V] {

  lazy val cacheItems = scala.collection.mutable.Set[V]() ++ getAllFn()

  def get(id : K) : V = {
    val item = getFn(id)
    cacheItems.retain(i => idFn(i) != id)
    cacheItems += item
    item
  }
  def getAll() : Set[V] = cacheItems.toSet

  def flush() {
    cacheItems.clear()
    cacheItems ++= getAll()
  }
}


class DefaultTitanTradeCache(edmTradeServices : TitanEdmTradeService)
  extends DefaultSimpleCache[TitanId, EDMPhysicalTrade](
      "EDM Trade cache",
      (t : EDMPhysicalTrade) => t.titanId,
      () => edmTradeServices.getAllCompletedTrades().toSet,
      (id : TitanId) => edmTradeServices.getTrade(id)) {

}



/**
 * Manage the trade store trades via changes at trade, quota, inventory and assignment level
 */
case class TitanTradeStoreManager(
  refData : TitanTacticalRefData,
  titanTradeStore : TitanTradeStore,
  edmTradeServices : TitanEdmTradeService,
  logisticsServices : TitanLogisticsServices) extends Log {

  type InventoryID = String

  def allStarlingTrades = titanTradeStore.allStarlingTrades()

  private lazy val edmTrades = {
    scala.collection.mutable.Set[EDMPhysicalTrade]() ++ edmTradeServices.getAllCompletedTrades()
  }

  private lazy val allInventory = logisticsServices.inventoryService.service.getAllInventory()

  private lazy val edmInventoryItems = {
    scala.collection.mutable.Map[InventoryID, EDMInventoryItem]() ++ allInventory.associatedInventory.map{inv => inv.id -> inv}
  }

  private val edmLogisticsQuotas = scala.collection.mutable.Set[EDMLogisticsQuota]()

  private def edmInventoryLeaves : List[EDMInventoryItem] = {
    val allInventoryIds = edmInventoryItems.keySet
    val parentIds = edmInventoryItems.values.flatMap{inv => inv.parentId.map(_.toString)}.toSet
    allInventoryIds.filterNot(parentIds).map(edmInventoryItems).toList
  }

  private def inventoryByQuotaID : Map[TitanId, List[EDMInventoryItem]] = {
    def quotaNames(inventory : EDMInventoryItem) : List[String] = inventory.purchaseAssignment.quotaName :: Option(inventory.salesAssignment).map(_.quotaName).toList
    def quotaNamesForInventory : List[(List[TitanId], EDMInventoryItem)] = edmInventoryItems.toList.map{case (id, inv) => (quotaNames(inv), inv)}.map(i => i._1.map(qn => TitanId(qn)) -> i._2)
    def quotaToInventory : List[(TitanId, EDMInventoryItem)] = quotaNamesForInventory.flatMap(i => i._1.map(x => (x -> i._2)))
    quotaToInventory.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
  }

  private def tradeForwardBuilder: PhysicalMetalForwardBuilder = {
    val logisticsQuotaByQuotaID = edmLogisticsQuotas.map(q => TitanId(q.quotaName) -> q).toMap
    new PhysicalMetalForwardBuilder(refData, inventoryByQuotaID, logisticsQuotaByQuotaID)
  }

  private def getTradeAndUpdateCache(titanTradeID : String) = {
    val newTrade = edmTradeServices.getTrade(TitanId(titanTradeID))

    log.info("trade state = " + newTrade.state)
    if (newTrade.state != CompletedTradeState) {
      log.error("Wrong state for trade " + newTrade.state)
    }

    edmTrades += newTrade
    newTrade
  }

  private def removeTradeFromCache(titanTradeID : String) {
    edmTrades.retain(_.titanId.value != titanTradeID)
  }

  private def updateInventoryCache(inventoryID : String) {
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

  private def removeInventoryFromCache(inventoryID : String) {

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

  /**
   * Returns list of trade ids that have changed value
   */
  def updateInventory(env : Environment, inventoryID : String, eventId : String) : TitanTradeUpdateResult = {

    try {

      val existingFwds = titanTradeStore.getAllForwards()
      updateInventoryCache(inventoryID)
      val tradeStoreResult = updateTradeStore(eventId)
      val updatedFwds = titanTradeStore.getAllForwards()

      val changedTitanTradeIds = (existingFwds.keySet ++ updatedFwds.keySet).flatMap{
        titanTradeId =>
          (existingFwds.get(titanTradeId), updatedFwds.get(titanTradeId)) match {
            case (Some(Right(oldFwd)), Some(Right(newFwd))) if oldFwd.costsAndIncomeQuotaValueBreakdown(env) != newFwd.costsAndIncomeQuotaValueBreakdown(env) => Some(titanTradeId)
            case _ => None
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
  }

  def updateTradeStore(eventId : String) : StoreResults = {
    val updatedStarlingTrades = edmTrades.flatMap(trade => tradeForwardBuilder.apply(trade, eventId))
    titanTradeStore.storeTrades(
      {trade : Trade => true},
      updatedStarlingTrades,
      new Timestamp
    )
  }

  /**
   * update / create a new trade, returns true if update changes valuation
   */
  def updateTrade(env : Environment, titanTradeID : String, eventId : String) : TitanTradeUpdateResult = {

    try {

      val existingFwd = titanTradeStore.getForward(titanTradeID)
      val newEDMTrade = getTradeAndUpdateCache(titanTradeID)

      val newStarlingTrades : List[Trade] = tradeForwardBuilder(newEDMTrade, eventId)

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
  }

  def deleteTrade(titanTradeID : String, eventId : String) : TitanTradeUpdateResult = {
    removeTradeFromCache(titanTradeID)
    TitanTradeUpdateResult(
      titanTradeStore.storeTrades(
        {trade => trade.titanTradeID == Some(titanTradeID)},
        Nil,
        new Timestamp
      ), Nil, false)
  }

  def deleteInventory(titanInventoryID : String, eventId : String) : TitanTradeUpdateResult = {
    removeInventoryFromCache(titanInventoryID)
    TitanTradeUpdateResult(
      titanTradeStore.storeTrades(
        {trade => trade.titanInventoryID == Some(titanInventoryID)},
        Nil,
        new Timestamp
      ), Nil, false)
  }

  /**
   * removal of a sales assignment only, we need to remove and refresh the inventory in the trade store
   * removal of inventory from cache will purge associated logistics quota and it will get updated
   * during the inventory update
   */
  def removeSalesAssignment(env : Environment, titanInventoryID : String, eventId : String) : TitanTradeUpdateResult = {
    removeInventoryFromCache(titanInventoryID)
    updateInventory(env, titanInventoryID, eventId)
  }
}

case class TitanTradeUpdateResult(
      tradeStoreResults : StoreResults,
      changedValueTitanTradeIds : List[String],
      hasValue : Boolean) {

  override def toString : String =
    "tradeStoreResults {%s}, changedValueTitanTradeIds {%s}, hasValue {%s}".format(tradeStoreResults.toString, changedValueTitanTradeIds.mkString(", "), hasValue)
}