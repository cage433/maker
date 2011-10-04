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

class TitanSystemOfRecord(
  titanTradeServices : TitanServices,
  logisticsServices : TitanLogisticsServices) extends SystemOfRecord with Log {

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {
    val edmTrades : List[EDMPhysicalTrade] = titanTradeServices.getAllCompletedTrades()
    val inventory = logisticsServices.inventoryService.service.getAllInventoryLeaves()
    println("No of edm trades = " + edmTrades.size + ", number of inventory items " + inventory.size)

    def quotaNames(inventory : EDMInventoryItem) : List[String] = inventory.purchaseAssignment.quotaName :: Option(inventory.salesAssignment).map(_.quotaName).toList

    val quotaNamesForInventory : List[(List[TitanId], EDMInventoryItem)] = inventory.map(i => (quotaNames(i), i)).map(i => i._1.map(qn => TitanId(qn)) -> i._2)
    val quotaToInventory : List[(TitanId, EDMInventoryItem)] = quotaNamesForInventory.flatMap(i => i._1.map(x => (x -> i._2)))
    val inventoryByQuotaID : Map[TitanId, List[EDMInventoryItem]] = quotaToInventory.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
    val logisticsQuotaByQuotaID : Map[TitanId, EDMLogisticsQuota] = Map() // this isn't currently implemented, probably best to complete after refactoring is completed

    val tradeForwardBuilder = new PhysicalMetalForwardBuilder(titanTradeServices, inventoryByQuotaID, logisticsQuotaByQuotaID)

    val allTrades : List[Trade] = edmTrades.flatMap {
      edmTrade =>
        try {
          tradeForwardBuilder(edmTrade)
        }
        catch {
          case e =>
            List(Trade(TradeID(edmTrade.titanId.value, TitanTradeSystem),
                TitanTradeAttributes.dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(edmTrade),
                new ErrorInstrument(e.getMessage)))
        }
    }

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


/*
// TODO, legacy converter stuf, to be removed post refactoring...
object TradeConverter{
  def apply(refData : TitanTacticalRefData, titanTradeCache : TitanTradeCache) : TradeConverter = {
    new TradeConverter(refData, titanTradeCache.quotaNameToTradeMap, titanTradeCache.quotaNameToQuotaMap)
  }
}

/**
 * EDM Inventory assignment to Starling "Trade" conversions
 *
 * ***** should be obsolete after refactoring *****
 */
class TradeConverter(refData : TitanTacticalRefData,
                     quotaNameToTradeMap : Map[String, EDMPhysicalTrade],
                     quotaNameToQuotaMap : Map[String, PhysicalTradeQuota]) {


  def toTrade(starlingTradeAssignment : StarlingTradeAssignment) : Trade = {
    toTrade(starlingTradeAssignment.id : String, starlingTradeAssignment.quotaName, starlingTradeAssignment.quantity, starlingTradeAssignment.assignmentID, starlingTradeAssignment.inventoryID)
  }

  // used when we have a concrete assignment
  def toTrade(assignment : EDMAssignment) : Trade = {
    toTrade(assignmentPrefix + assignment.oid.contents.toString, assignment.quotaName, assignment.quantity, Some(assignment.oid.contents.toString), Some(assignment.inventoryId.toString))
  }

  def toTrade(id : String, quotaName : String, quantity : Quantity, assignmentID : Option[String], inventoryID : Option[String]) : Trade = {

    val quotaDetail = quotaNameToQuotaMap(quotaName).detail
    require(quotaDetail.deliverySpecs.size == 1, "Require exactly one delivery spec")

    val edmTrade = quotaNameToTradeMap(quotaName)
    val groupCompany = refData.groupCompaniesByGUID.get(edmTrade.groupCompany).map(_.name).getOrElse("Unknown Group Company")
    val counterparty = refData.counterpartiesByGUID(edmTrade.counterparty.counterparty).name



    val deliverySpec = quotaDetail.deliverySpecs.head
    val (shape, grade) = deliverySpec.materialSpec match {
      case rms : com.trafigura.edm.materialspecification.RefinedMetalSpec => (
        refData.shapesByGUID(rms.shape),
        refData.gradeByGUID(rms.grade)
      )
      case _ => throw new Exception("Expected RefinedMetalSpec, recieved " + quotaDetail.deliverySpecs.head.materialSpec )
    }
    require(deliverySpec.deliveryLocations.size == 1, "Need exactly one delivery location")
    val contractLocation = refData.locationsByGUID(deliverySpec.deliveryLocations.head.location)
    val destinationLocation = refData.destLocationsByGUID(deliverySpec.destinationLocation)
    val contractFinalised = edmTrade.contractFinalised.toString

    val tolerance = deliverySpec.tolerance match {
      case tol : PercentageTolerance => tol
      case _ => throw new Exception("Unsupported tolerance")
    }

    def getTolerancePercentage(percentage : Option[Double]) : Percentage = percentage match {
      case Some(percentage) => Percentage(percentage)
      case _ => Percentage(0.0)
    }

    val tolerancePlus = getTolerancePercentage(tolerance.plus.amount)
    val toleranceMinus = getTolerancePercentage(tolerance.minus.amount)

    def getDate(ds : DateSpec) : LocalDate = ds match {
      case date : com.trafigura.edm.shared.types.Date => date.value
      case _ => throw new Exception("Unsupported DateSpec type")
    }

    val contractDeliveryDay = Day.fromLocalDate(getDate(deliverySpec.contractDeliveryDay))
    val expectedSales = Day.fromLocalDate(getDate(quotaDetail.expectedSales))
    val quotaId = quotaName

    val attributes : TradeAttributes = TitanTradeAttributes(
      assignmentID,
      quotaId,
      Option(edmTrade.titanId.value).getOrElse("NULL TITAN TRADE ID"),
      inventoryID,
      groupCompany,
      edmTrade.comments,
      Day.fromLocalDate(edmTrade.submitted.toLocalDate),
      shape.name,
      grade.name,
      contractLocation.name,
      destinationLocation.name,
      contractFinalised,
      tolerancePlus,
      toleranceMinus,
      contractDeliveryDay,
      expectedSales
    )

    import EDMConversions._

    val metal : Metal = refData.edmMetalByGUID(quotaDetail.deliverySpecs.head.materialSpec.asInstanceOf[CommoditySpec].commodity)
    val priceSpecConverter = EDMPricingSpecConverter(metal, refData.futuresExchangeByID)

    // todo - this is the wrong quantity at the assignment level??
    val deliveryQuantity = quantity // = quotaDetail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum

    val deliveryDay = Day.fromLocalDate(quotaDetail.deliverySpecs.head.contractDeliveryDay.asInstanceOf[Date].value)

    val tradeable : Tradeable = PhysicalMetalAssignment(
      assignmentID.getOrElse(""),
      metal.name,
      deliveryQuantity,
      deliveryDay,
      priceSpecConverter.fromEdmPricingSpec(deliveryDay, deliveryQuantity, quotaDetail.pricingSpec),
      benchmarkDeliveryDay = None,
      benchmarkPricingSpec = None,
      isPurchase = false,
      inventoryID = null,
      inventoryQuantity = null,
      quotaQuantity = null
    )

    val costs : List[Costs] = Nil // todo

    if (edmTrade.submitted == null) {
      println("NULL DATE  " + edmTrade.titanId)
    }

    Trade(
      TradeID(id, TitanTradeSystem),
      Day.fromLocalDate(edmTrade.submitted.toLocalDate),
      counterparty,
      attributes,
      tradeable,
      costs
      )
  }
}

*/
