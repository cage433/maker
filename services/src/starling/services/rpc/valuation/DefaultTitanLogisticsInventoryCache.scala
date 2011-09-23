package starling.services.rpc.valuation

import starling.props.Props
import java.lang.Exception
import starling.titan._
import starling.services.rpc.logistics._
import starling.daterange.Timestamp
import starling.instrument.Trade
import starling.utils.conversions.RichMapWithErrors._
import com.trafigura.edm.logistics.inventory.{EDMLogisticsQuota, EDMInventoryItem}
import starling.utils.{Log, Stopwatch}
import com.trafigura.edm.shared.types.Quantity


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
    titanTradeStore : Option[TitanTradeStore]) extends GenericServiceDataCache[EDMInventoryItem, String] with Log {

  // internal maps that for the cache lookup
  protected var inventoryMap : Map[String, EDMInventoryItem] = Map[String, EDMInventoryItem]().withException()
  protected var assignmentIDtoInventoryIDMap : Map[String, String] = Map[String, String]().withException()
  protected var logisticsIDtoQuotaMap : Map[String, EDMLogisticsQuota] = Map[String, EDMLogisticsQuota]().withException()

  private def getAllInventory() = try {
    //titanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
    titanLogisticsServices.inventoryService.service.getAllInventory()
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }
  private def getInventoryById(id : Int) = try {
    titanLogisticsServices.inventoryService.service.getInventoryById(id) //.associatedInventory.head
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
  private def deleteFromTradeStore(inventoryID : String) {
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
    //writeToTradeStore(inventoryID)
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
    val inventory = edmInventoryResult.associatedInventory
    inventoryMap = inventory.map(i => (i.oid.contents.toString, i)).toMap
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
      val inventoryResponse = getInventoryById(id.toInt)
      val inventoryList = inventoryResponse.associatedInventory
      assert(inventoryList.size <= 1, "Incorrect number of inventory items returned, expected exactly 1, got " + inventoryList.size)
      val quotasList = inventoryResponse.associatedQuota
      logisticsIDtoQuotaMap ++= quotasList.map(q => q.quotaId.toString -> q).toMap
      inventoryResponse.associatedInventory.headOption match {
        case Some(item) => {
          inventoryMap += item.oid.contents.toString -> item
          addInventoryAssignments(id)

          titanTradeStore match {
            case Some(tradeStore) => {
              try {
                writeToTradeStore(id)

                val logisticsUnallocatedQuotas : List[StarlingTradeAssignment] = quotasList.flatMap(logisticsQuota => {
                  logisticsIDtoQuotaMap.get(logisticsQuota.quotaId.toString) match {
                    //val quotas.map(q => q.quotaName -> quota).toMap
                    case Some(_) => None
                    case None => {
                      Some(StarlingTradeAssignment(logisticsQuota.quotaId.toString, logisticsQuota.quotaQuantity))
                    }
                  }
                })
                val tradeConverter = TradeConverter(refData, titanTradeCache)
                logisticsUnallocatedQuotas.foreach(logisticsUnallocatedQuota => {
                  val trade = tradeConverter.toTrade(logisticsUnallocatedQuota)
                  tradeStore.storeTrades((trade) => logisticsUnallocatedQuotas.map(_.id).contains(trade.tradeID), List(trade), new Timestamp())
                })
                inventoryMap(id)
              }
              catch {
                case ex => {
                  Log.error("Unable to store new trades in trade store, ", ex)
                  throw ex
                }
              }
            }
            case None => {
              val msg = "Missing inventory from service, requested by id item %s, this missing item in the cache may lead to unexpected results".format(id)
              Log.warn(msg)
              throw new Exception("Inventory cache miss - " + msg)
            }
          }
        }
      }
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

  def getInventoryByIds(ids : List[String]) = getAllInventory().associatedInventory.filter(i => ids.exists(_ == i.oid.contents.toString))
}

object DefaultTitanLogisticsInventoryCache {
  def apply(props : Props,titanTradeCache : TitanTradeCache, refData : TitanTacticalRefData, titanTradeStore : Option[TitanTradeStore]) : DefaultTitanLogisticsInventoryCache = {
    new DefaultTitanLogisticsInventoryCache(DefaultTitanLogisticsServices(props), titanTradeCache, refData, titanTradeStore)
  }
}

