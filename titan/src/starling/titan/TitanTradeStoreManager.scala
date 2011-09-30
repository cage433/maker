package starling.titan

import starling.curves.Environment
import starling.instrument.physical.PhysicalMetalForward


/**
 * Manage the trade store trades via changes at trade, quota, inventory and assignment level
 */
case class TitanTradeStoreManager(
  refData : TitanTacticalRefData,
  titanTradeStore : TitanTradeStore,
  edmTradeServices : TitanEdmTradeService,
  logisticsServices : TitanLogisticsServices) {

  def forward(id : String) : PhysicalMetalForward = {

    val allTrades = titanTradeStore.readLatestVersionOfAllTrades()

    val trades = allTrades.filter(t => t._1 == id)

//    val forward = forwardBuilder.mkForward(trades)

    titanTradeStore.getForward(id)
  }

  def forwardsByInventory(inventoryID : String) : List[PhysicalMetalForward] = {

//    val allTrades = titanTradeStore.readLatestVersionOfAllTrades()
//
//    val trades = allTrades.filter(t => t._2.trade.inventoryID == inventoryID)groupBy(t => t._1)
//
//    val fwds = trades.map((k, v) => forwardBuilder(v))
//
//    titanTradeStore.getForwardsByInventory(inventoryID)
    null
  }

  /**
   * Returns list of trade ids that have changed
   */
  def updateInventory(env : Environment, inventoryID : String) : List[String] = {
    val oldForwards = forwardsByInventory(inventoryID).map{fwd => fwd.tradeID -> fwd}.toMap
    // update trade store
    val newForwards = forwardsByInventory(inventoryID).map{fwd => fwd.tradeID -> fwd}.toMap

    (oldForwards.keySet ++ newForwards.keySet).filter {
      id =>
        (oldForwards.get(id), newForwards.get(id)) match {
          case (Some(fwd1), Some(fwd2)) => fwd1.costsAndIncomeQuotaValueBreakdown(env) != fwd2.costsAndIncomeQuotaValueBreakdown(env)
          case _ => true
        }
    }.toList
  }

  def deleteInventory(id : String) {

  }

  /**
   * Returns true if update changes valuation
   */
  def updateTrade(env : Environment, id : String) : Boolean = {

    val oldForward = titanTradeStore.getForward(id)
    // update trade store  - filter if not completed in Titan
    val newForward = titanTradeStore.getForward(id)

    (oldForward.costsAndIncomeQuotaValueBreakdown(env) != newForward.costsAndIncomeQuotaValueBreakdown(env))
  }


  def deleteTrade(id : String) {

  }
}

