package starling.titan

import starling.systemofrecord.SystemOfRecord
import starling.daterange.Day
import starling.pivot.Field
import starling.instrument.{Trade, TradeAttributes}
import java.lang.UnsupportedOperationException
import starling.instrument.ErrorInstrument
import com.trafigura.edm.logistics.inventory._
import starling.utils.Log
import com.trafigura.edm.trademgmt.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.instrument.physical._
import starling.quantity.{Quantity, Percentage}
import starling.titan.EDMConversions._
import runtime.ScalaRunTime


// represent logistics inventory values needed for pricing metals
case class Inventory(item: InventoryItem) {

  // if we've reached the receipted inventory status (or later status) we can use the assignment qty, otherwise assignment qty it's not yet valid
  val receivedQuantity: Option[Quantity] = {
    item.status match {
      case ExpectedInventoryItemStatus => None
      case SplitInventoryItemStatus | CancelledInventoryItemStatus =>
        throw new Exception("Unexpected inventory status '" + item.status + "' for inventory item " + item.oid.contents)
      case _ => Some(item.purchaseAssignment.quantity)
    }
  }

  // take the receipted qty (if available) to reflect logistics receipt qty, otherwise if not yet received then use the inventory qty as best estimate
  def purchaseAssignmentQuantity: Quantity = receivedQuantity.getOrElse(item.quantity)
  def isAllocated = Option(item.salesAssignment).isDefined
  def currentQuantity = item.quantity
  def id = item.oid.toString
}


class TitanSystemOfRecord(manager : TitanTradeStoreManager)
  extends SystemOfRecord with Log {

  // TODO Louis - 21.12.2011
  // This should be fixed so that it doesn't store trades in the tradestore
  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {
    val tradeStoreResults = manager.updateTradeStore("allTrade import request")
    val allTrades = manager.allStarlingTrades

    val duplicates = allTrades.groupBy(_.tradeID).filter(kv => kv._2.size > 1)
    assert(duplicates.isEmpty, "duplicates found: \n" + duplicates.mkString("\n"))

    allTrades.map(f)

    val tradeErrors = allTrades.map(_.tradeable).collect{case ErrorInstrument(err) => err}

    log.info("tradeStoreResults, inserted %d, updated %d, deleted %d, error trades %d".format(tradeStoreResults.inserted, tradeStoreResults.updated, tradeStoreResults.deleted, tradeErrors.size))

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
  groupCompany : String,
  comment : String,
  submitted : Day,
  shape : String,
  contractFinalised : String,
  tolerancePlus : Percentage,
  toleranceMinus : Percentage,
  eventID : String) extends TradeAttributes {

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
    toleranceMinus_str -> toleranceMinus,
    eventID_str -> eventID
  )

  override def createFieldValues = details.map{
    case (k, v) => Field(k) -> v
  }

  override def hashCode() = ScalaRunTime._hashCode(copy(eventID = "", comment = ""))

  // We don't want to use eventID in hashCode and equals as it changes every time a trade
  // is published on the bus, even if the trade has not changed. If we include it in hashCode
  // and equals it looks like the trade has changed and we store a new revision.
  override def equals(obj:Any) = obj match {
    case other:TitanTradeAttributes => {
      ScalaRunTime._equals(this, other.copy(eventID = this.eventID, comment = this.comment))
    }
    case _ => false
  }
}

object TitanTradeAttributes{
  val dummyDate = Day(1980, 1, 1)
  def errorAttributes(edmTradeId : String, evntID : String) = {
    TitanTradeAttributes(
      quotaID = "",
      quotaQuantity = Quantity.NULL,
      titanTradeID = edmTradeId,
      groupCompany = "",
      comment = "",
      submitted = dummyDate,
      shape = "",
      contractFinalised = "",
      tolerancePlus = Percentage(0),
      toleranceMinus = Percentage(0),
      eventID = evntID
    )
  }
}

