package starling.services.rpc.valuation

import starling.props.Props
import java.lang.Exception
import starling.titan._
import starling.services.rpc.logistics._
import starling.utils.Stopwatch
import com.trafigura.edm.logistics.inventory.EDMInventoryItem
import starling.daterange.Timestamp
import starling.instrument.Trade


case class TitanLogisticsInventoryCache(
    titanLogisticsServices : TitanLogisticsServices,
    titanTradeCache : TitanTradeCache,
    refData : TitanTacticalRefData,
    titanTradeStore : Option[TitanTradeStore]) {

  protected var inventoryMap : Map[String, EDMInventoryItem] = Map[String, EDMInventoryItem]()
  protected var assignmentIDtoInventoryIDMap : Map[String, String] = Map[String, String]()

  private def getAll() = try {
      titanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }
  private def getById(id : Int) = try {
    titanLogisticsServices.inventoryService.service.getInventoryById(id).associatedInventory.head
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }

  private def tradesForInventory(inventoryID: String): List[Trade] = {
    val tradeConverter = TradeConverter(refData, titanTradeCache)
    getAssignmentsForInventory(inventoryID).map(assignment => tradeConverter.toTrade(assignment))
  }
  private def writeToTradeStore(inventoryID : String) {
    titanTradeStore match {
      case Some(tradeStore) => {
        val trades : Seq[Trade] = tradesForInventory(inventoryID)
        val tradeIDs = trades.map(_.tradeID)
        tradeStore.storeTrades((trade) => tradeIDs.contains(trade.tradeID), trades, new Timestamp())
      }
      case None =>
    }
  }
  private def deleteFromTradeStore(inventoryID : String){
    titanTradeStore match {
      case Some(tradeStore) => {
        val tradeIDs = tradesForInventory(inventoryID).map(_.tradeID)
        tradeStore.storeTrades((trade) => tradeIDs.contains(trade.tradeID), Nil, new Timestamp())
      }
      case None =>
    }
  }

  def getAssignmentsForInventory(id: String) = {
    val inventory = getInventory(id)
    inventory.purchaseAssignment :: Option(inventory.salesAssignment).toList
  }
  def removeInventory(inventoryID : String) {
    inventoryMap = inventoryMap - inventoryID
    assignmentIDtoInventoryIDMap.filter{ case (_, value) => value != inventoryID}
    deleteFromTradeStore(inventoryID)
  }

  def addInventory(inventoryID : String) {
    inventoryMap += inventoryID -> getInventory(inventoryID)
    addInventoryAssignments(inventoryID)
    writeToTradeStore(inventoryID)
  }

  def addInventoryAssignments(inventoryID : String) {
    val item = inventoryMap(inventoryID)
    val assignmentToInventoryMapItems = (item.purchaseAssignment.oid.contents.toString -> inventoryID) :: Option(item.salesAssignment).toList.map(_.oid.contents.toString -> inventoryID)
    assignmentIDtoInventoryIDMap ++= assignmentToInventoryMapItems
  }

  // Read all inventory from Titan and blast our cache
  def updateMap() {
    val sw = new Stopwatch()
    val edmInventoryResult = getAll()
    inventoryMap = edmInventoryResult.map(i => (i.oid.contents.toString, i)).toMap
    inventoryMap.keySet.foreach(addInventoryAssignments)
  }

  def getAllInventory() : List[EDMInventoryItem] = {
    if (inventoryMap.size > 0) {
      inventoryMap.values.toList
    }
    else {
      updateMap()
      inventoryMap.values.toList
    }
  }

  def getInventory(id: String) : EDMInventoryItem = {
    if (inventoryMap.contains(id)) {
      inventoryMap(id)
    }
    else {
      val item = getById(id.toInt)
      inventoryMap += item.oid.contents.toString -> item
      addInventoryAssignments(id)
      inventoryMap(id)
    }
  }

  def inventoryIDFromAssignmentID(id: String): String = {
    if (!assignmentIDtoInventoryIDMap.contains(id))
      updateMap()
    assignmentIDtoInventoryIDMap.get(id) match {
      case Some(id) => id
      case None => throw new Exception("Missing inventory " + id)
    }
  }

  def getInventoryByIds(ids : List[String]) = getAllInventory().filter(i => ids.exists(_ == i.oid.contents.toString))
}

object TitanLogisticsInventoryCache {
  def apply(props : Props,titanTradeCache : TitanTradeCache, refData : TitanTacticalRefData, titanTradeStore : Option[TitanTradeStore]) : TitanLogisticsInventoryCache = {
    new TitanLogisticsInventoryCache(DefaultTitanLogisticsServices(props), titanTradeCache, refData, titanTradeStore)
  }
}

