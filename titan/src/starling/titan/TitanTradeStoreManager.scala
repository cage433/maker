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


/**
 * Manage the trade store trades via changes at trade, quota, inventory and assignment level
 */
case class TitanTradeStoreManager(
  refData : TitanTacticalRefData,
  titanTradeStore : TitanTradeStore,
  edmTradeServices : TitanEdmTradeService,
  logisticsServices : TitanLogisticsServices) extends Log {

  type InventoryID = String

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

  def inventoryByQuotaID : Map[TitanId, List[EDMInventoryItem]] = {
    def quotaNames(inventory : EDMInventoryItem) : List[String] = inventory.purchaseAssignment.quotaName :: Option(inventory.salesAssignment).map(_.quotaName).toList
    def quotaNamesForInventory : List[(List[TitanId], EDMInventoryItem)] = edmInventoryItems.toList.map{case (id, inv) => (quotaNames(inv), inv)}.map(i => i._1.map(qn => TitanId(qn)) -> i._2)
    def quotaToInventory : List[(TitanId, EDMInventoryItem)] = quotaNamesForInventory.flatMap(i => i._1.map(x => (x -> i._2)))
    quotaToInventory.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
  }

  private def tradeForwardBuilder: PhysicalMetalForwardBuilder = {
    val logisticsQuotaByQuotaID = edmLogisticsQuotas.map(q => TitanId(q.quotaName) -> q).toMap
    new PhysicalMetalForwardBuilder(refData, inventoryByQuotaID, logisticsQuotaByQuotaID)
  }

  def allStarlingTrades = titanTradeStore.allStarlingTrades()

  private def getTradeAndUpdateCache(titanTradeID : String) = {
    val newTrade = edmTradeServices.getTrade(TitanId(titanTradeID))

    log.info("trade state = " + newTrade.state)
    if (newTrade.state != CompletedTradeState) {
      log.error("Wrong state for trade " + newTrade.state)
    }

    edmTrades += newTrade
    newTrade
  }

  def removeTradeFromCache(titanTradeID : String) {
    edmTrades.retain(_.titanId.value != titanTradeID)
  }

  def updateInventoryCache(inventoryID : String) {
    val logisticsResponse = logisticsServices.inventoryService.service.getInventoryById(inventoryID.toInt)
    assert(logisticsResponse.associatedInventory.size == 1, "Expected a single piece of inventory")
    val newInventory = logisticsResponse.associatedInventory.head

    val newQuotas = logisticsResponse.associatedQuota
    assert(newQuotas.size >= 1 && newQuotas.size <= 2, "Expected one or two quotas")

    log.info("inventory update, got inventory:\n %s, \n%s \nwith quotas \n%s\n".format(inventoryID, newInventory, newQuotas.map(q => (q.quotaName, q.fullyAllocated))))

    edmInventoryItems += (newInventory.id -> newInventory)
    edmLogisticsQuotas --= newQuotas
    edmLogisticsQuotas ++= newQuotas
  }

  def removeInventoryFromCache(inventoryID : String) {
    edmInventoryItems -= inventoryID
  }

  /**
   * Returns list of trade ids that have changed value
   */
  def updateInventory(env : Environment, inventoryID : String) : List[String] = {

    try {

      val existingFwds = titanTradeStore.getAllForwards()
      updateInventoryCache(inventoryID)
      updateTradeStore()
      val updatedFwds = titanTradeStore.getAllForwards()

      val changedTitanTradeIds = (existingFwds.keySet ++ updatedFwds.keySet).flatMap{
        titanTradeId =>
          (existingFwds.get(titanTradeId), updatedFwds.get(titanTradeId)) match {
            case (Some(Right(oldFwd)), Some(Right(newFwd))) if oldFwd.costsAndIncomeQuotaValueBreakdown(env) != newFwd.costsAndIncomeQuotaValueBreakdown(env) => Some(titanTradeId)
            case _ => None
          }
      }.toList

      changedTitanTradeIds
    }
    catch {
      case ex => {
        Log.error("update trade failed ", ex)
        throw ex
      }
    }
  }

  def updateTradeStore() {
    val updatedStarlingTrades = edmTrades.flatMap(tradeForwardBuilder.apply)
    titanTradeStore.storeTrades(
      {trade : Trade => true},
      updatedStarlingTrades,
      new Timestamp
    )
  }
  /**
   * update / create a new trade, returns true if update changes valuation
   */
  def updateTrade(env : Environment, titanTradeID : String) : Boolean = {

    try {

      val existingFwd = titanTradeStore.getForward(titanTradeID)
      val newEDMTrade = getTradeAndUpdateCache(titanTradeID)

      val newStarlingTrades : List[Trade] = tradeForwardBuilder(newEDMTrade)

      titanTradeStore.storeTrades(
        {trade : Trade => trade.attributes match {
          case t : TitanTradeAttributes => t.titanTradeID == Some(titanTradeID)
          case _ => throw new Exception("Unexpected trade attributes type for trade " + trade)

        }},
        newStarlingTrades,
        new Timestamp
      )

      val updatedFwd = titanTradeStore.getForward(titanTradeID)

      val valueChanged = (existingFwd, updatedFwd) match {
        case (Right(oldFwd), Right(newFwd)) => oldFwd.costsAndIncomeQuotaValueBreakdown(env) != newFwd.costsAndIncomeQuotaValueBreakdown(env)
        case (Left(_), Left(_)) => false
        case _ => true
      }

      valueChanged
    }
    catch {
      case ex => {
        Log.error("update trade failed ", ex)
        throw ex
      }
    }
  }

  def deleteTrade(titanTradeID : String) {
    removeTradeFromCache(titanTradeID)
    titanTradeStore.storeTrades(
      {trade => trade.titanTradeID == Some(titanTradeID)},
      Nil,
      new Timestamp
    )
  }

  def deleteInventory(titanInventoryID : String) {
    removeInventoryFromCache(titanInventoryID)
    titanTradeStore.storeTrades(
      {trade => trade.titanInventoryID == Some(titanInventoryID)},
      Nil,
      new Timestamp
    )
  }
}
