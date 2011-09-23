package starling.services.rpc.valuation

import starling.props.Props
import java.lang.Exception
import starling.titan._
import starling.services.rpc.logistics._
import starling.utils.Stopwatch
import com.trafigura.edm.logistics.inventory.EDMInventoryItem
import starling.daterange.Timestamp
import starling.instrument.Trade
import starling.utils.conversions.RichMapWithErrors._

/**
 * Basic service cache trait that provides getByID and getAll + addByID and remove, expected to handle cache misses
 *   via the supplied implementation (i.e. backed by a service etc)
 */
trait GenericServiceDataCache[T, K] {
  def getByID(id : K) : T
  def getAll() : List[T]

  def addByID(item : K)
  //def add(item : T) // not currently required, possibly never actually needed?
  def remove(id : K)
  //def update(item : T)
}

case class DefaultTitanLogisticsInventoryCache(
    titanLogisticsServices : TitanLogisticsServices,
    titanTradeCache : TitanTradeCache,
    refData : TitanTacticalRefData,
    titanTradeStore : Option[TitanTradeStore]) extends GenericServiceDataCache[EDMInventoryItem, String] {

  protected var inventoryMap : Map[String, EDMInventoryItem] = Map[String, EDMInventoryItem]().withException()
  protected var assignmentIDtoInventoryIDMap : Map[String, String] = Map[String, String]().withException()

  private def getAllInventory() = try {
      titanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }
  private def getInventoryById(id : Int) = try {
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
    val inventory = getByID(id)
    inventory.purchaseAssignment :: Option(inventory.salesAssignment).toList
  }
  def remove(inventoryID : String) {
    inventoryMap = inventoryMap - inventoryID
    assignmentIDtoInventoryIDMap.filter{ case (_, value) => value != inventoryID}
    deleteFromTradeStore(inventoryID)
  }

  def addByID(inventoryID : String) {
    inventoryMap += inventoryID -> getByID(inventoryID)
    addInventoryAssignments(inventoryID)
    writeToTradeStore(inventoryID)
  }

  private def addInventoryAssignments(inventoryID : String) {
    val item = inventoryMap(inventoryID)
    val assignmentToInventoryMapItems = (item.purchaseAssignment.oid.contents.toString -> inventoryID) :: Option(item.salesAssignment).toList.map(_.oid.contents.toString -> inventoryID)
    assignmentIDtoInventoryIDMap ++= assignmentToInventoryMapItems
  }

  // Read all inventory from Titan and blast our cache
  private def updateMap() {
    val sw = new Stopwatch()
    val edmInventoryResult = getAllInventory()
    inventoryMap = edmInventoryResult.map(i => (i.oid.contents.toString, i)).toMap
    inventoryMap.keySet.foreach(addInventoryAssignments)
  }

  def getAll() : List[EDMInventoryItem] = {
    if (inventoryMap.size > 0) {
      inventoryMap.values.toList
    }
    else {
      updateMap()
      inventoryMap.values.toList
    }
  }

  def getByID(id: String) : EDMInventoryItem = {
    if (inventoryMap.contains(id)) {
      inventoryMap(id)
    }
    else {
      val item = getInventoryById(id.toInt)
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

object DefaultTitanLogisticsInventoryCache {
  def apply(props : Props,titanTradeCache : TitanTradeCache, refData : TitanTacticalRefData, titanTradeStore : Option[TitanTradeStore]) : DefaultTitanLogisticsInventoryCache = {
    new DefaultTitanLogisticsInventoryCache(DefaultTitanLogisticsServices(props), titanTradeCache, refData, titanTradeStore)
  }
}

