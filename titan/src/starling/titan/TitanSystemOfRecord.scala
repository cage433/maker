package starling.titan

import starling.systemofrecord.SystemOfRecord
import starling.daterange.Day
import starling.pivot.Field
import starling.instrument.{Trade, TradeID, TradeAttributes}
import starling.db.TitanTradeSystem
import java.lang.UnsupportedOperationException
import starling.instrument.ErrorInstrument
import com.trafigura.edm.logistics.inventory._
import com.trafigura.edm.shared.types.TitanId
import starling.utils.Log
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.instrument.physical._
import starling.quantity.{Quantity, Percentage}
import starling.titan.EDMConversions._


case class Inventory(item: EDMInventoryItem) {
  val receivedQuantity: Option[Quantity] = {
    item.status match {
      case ExpectedEDMInventoryItemStatus => None
      case SplitEDMInventoryItemStatus | CancelledEDMInventoryItemStatus => throw new Exception("Unexpected inventory status " + item.status)
      case _ => Some(item.purchaseAssignment.quantity)
    }
  }
  def assignmentQuantity: Quantity = receivedQuantity.getOrElse(item.quantity)
  def isAllocated = Option(item.salesAssignment).isDefined
  def currentQuantity = item.quantity
  def id = item.oid.toString
}

class TitanSystemOfRecord(manager : TitanTradeStoreManager)
  extends SystemOfRecord with Log {

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {
    manager.updateTradeStore
    val allTrades = manager.allStarlingTrades
//    // manager.readAll
//    // val allTrades = manager.allTrades
//    val edmTrades : List[EDMPhysicalTrade] = titanTradeServices.getAllCompletedTrades()
//    val inventory = logisticsServices.inventoryService.service.getAllInventoryLeaves()
//    println("No of edm trades = " + edmTrades.size + ", number of inventory items " + inventory.size)
//
//    def quotaNames(inventory : EDMInventoryItem) : List[String] = inventory.purchaseAssignment.quotaName :: Option(inventory.salesAssignment).map(_.quotaName).toList
//
//    val quotaNamesForInventory : List[(List[TitanId], EDMInventoryItem)] = inventory.map(i => (quotaNames(i), i)).map(i => i._1.map(qn => TitanId(qn)) -> i._2)
//    val quotaToInventory : List[(TitanId, EDMInventoryItem)] = quotaNamesForInventory.flatMap(i => i._1.map(x => (x -> i._2)))
//    val inventoryByQuotaID : Map[TitanId, List[EDMInventoryItem]] = quotaToInventory.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
//    val logisticsQuotaByQuotaID : Map[TitanId, EDMLogisticsQuota] = Map() // this isn't currently implemented, probably best to complete after refactoring is completed
//
//    val tradeForwardBuilder = new PhysicalMetalForwardBuilder(titanTradeServices, inventoryByQuotaID, logisticsQuotaByQuotaID)
//
//    val allTrades : List[Trade] = edmTrades.flatMap {
//      edmTrade =>
//        try {
//          tradeForwardBuilder(edmTrade)
//        }
//        catch {
//          case e =>
//            List(Trade(TradeID(edmTrade.titanId.value, TitanTradeSystem),
//                TitanTradeAttributes.dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(edmTrade),
//                new ErrorInstrument(e.getMessage)))
//        }
//    }

    val (worked, failed) = allTrades.map(_.tradeable).partition{ case a : PhysicalMetalAssignment => true; case _ => false }

    log.info("Failed : \n" + failed.mkString("\n"))
    log.info("Worked : \n" + worked.mkString("\n"))

    allTrades.map(f)

    val dups = allTrades.groupBy(_.tradeID).filter(kv => kv._2.size > 1)
    log.warn("dups found: \n" + dups.mkString("\n"))

    val tradeErrors = allTrades.map(_.tradeable).collect{case ErrorInstrument(err) => err}
    (tradeErrors.size, tradeErrors.toSet)
  }

  // not used??
  def trade(id: String)(f: (Trade) => Unit) {}

  protected def readers = throw new UnsupportedOperationException()
}

case class TitanTradeAttributes(
  quotaID : String,
  quotaQuantity : Quantity,
  titanTradeID : String,
  inventoryID : Option[String],
  groupCompany : String,
  comment : String,
  submitted : Day,
  shape : String,
  contractFinalised : String,
  tolerancePlus : Percentage,
  toleranceMinus : Percentage
) extends TradeAttributes {

  if (quotaID == null) {
    println("quota was null")
  }
  require(quotaID != null, "quotaID cannot be null")
  require(titanTradeID != null, "titanTradeID cannot be null")

  import TitanTradeStore._

  def details = Map(
    quotaID_str -> quotaID,
    quotaQuantity_str -> quotaQuantity,
    titanTradeID_str -> titanTradeID,
    groupCompany_str -> groupCompany,
    comment_str -> comment,
    submitted_str -> submitted,
    shape_str -> shape,
    contractFinalised_str -> contractFinalised,
    tolerancePlus_str -> tolerancePlus,
    toleranceMinus_str -> toleranceMinus
  ) ++  inventoryID.map{id => inventoryID_str -> id}

  override def createFieldValues = details.map{
    case (k, v) => Field(k) -> v
  }
}

object TitanTradeAttributes{
  val dummyDate = Day(1980, 1, 1)
  def errorAttributes(edmTrade : EDMPhysicalTrade) = {
    val titanTradeID = edmTrade.titanId.value

    TitanTradeAttributes(
      quotaID = "",
      quotaQuantity = Quantity.NULL,
      titanTradeID = titanTradeID,
      inventoryID = None,
      groupCompany = "",
      comment = "",
      submitted = dummyDate,
      shape = "",
      contractFinalised = "",
      tolerancePlus = Percentage(0),
      toleranceMinus = Percentage(0)
    )
  }
}

