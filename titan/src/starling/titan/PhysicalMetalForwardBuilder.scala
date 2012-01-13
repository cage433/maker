package starling.titan

import org.joda.time.LocalDate
import com.trafigura.edm.trademgmt.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.trademgmt.physicaltradespecs.{DeliverySpec, QuotaDetails}
import com.trafigura.edm.trademgmt.materialspecification.CommoditySpec
import com.trafigura.trademgmt.internal.refinedmetal.{DestinationLocation, Location}
import com.trafigura.tradinghub.support.GUID
import com.trafigura.edm.common.units.{DateSpec, PercentageTolerance, TitanId}
import starling.titan.EDMConversions._
import starling.utils.Log
import starling.daterange.Day
import starling.instrument.physical._
import starling.quantity.{Quantity, Percentage}
import starling.db.TitanTradeSystem
import starling.instrument.{ErrorInstrument, TradeID, Trade}
import starling.marketdata._
import starling.market.{NeptuneCommodity, Commodity}
import com.trafigura.edm.logistics.inventory._

case class TradeManagementQuotaDetails(
  isPurchase : Boolean,
  counterparty : String,
  commodity : NeptuneCommodity,
  contractDeliveryDay : Day,
  benchmarkDeliveryDay : Day,
  contractPricingSpec : TitanPricingSpec,
  contractLocation : ContractualLocationCode,
  benchmarkCountry : NeptuneCountryCode,
  contractIncoTerm : IncotermCode,
  benchmarkIncoTerm : Option[IncotermCode],
  grade : GradeCode,
  attributes : TitanTradeAttributes
){
  def titanTradeID = attributes.titanTradeID
  def quotaID = attributes.quotaID
}

object TradeManagementQuotaDetails{
  private def deliverySpec(detail : QuotaDetails) = {
    assert(detail.deliverySpecs.size == 1, "Requires there to be a single delivery spec")
    detail.deliverySpecs.head
  }

  def buildListOfQuotaDetails(refData: TitanTacticalRefData, trade : EDMPhysicalTrade, eventID : String) : List[TradeManagementQuotaDetails] = {
    trade.quotas.map{q => apply(refData, trade, q.detail, eventID)}
  }

  def apply(refData: TitanTacticalRefData, trade : EDMPhysicalTrade, detail : QuotaDetails, eventID : String) : TradeManagementQuotaDetails = {
    import refData._
    val deliverySpec_ = deliverySpec(detail)
    val commodityGUID: GUID = deliverySpec_.materialSpec.asInstanceOf[CommoditySpec].commodity
    val commodityName = edmMetalByGUID(commodityGUID)
    val commodity = {
      Commodity.neptuneCommodityFromNeptuneName(edmMetalByGUID(commodityGUID).name) match {
        case Some(c) => c
        case _ => throw new Exception("Missing neptune commodity %s (edm commodity %s)".format(commodityGUID, commodityName))
      }
    }
    val (contractDeliveryDay, benchmarkDeliveryDay) = {
      def getDate(ds : DateSpec) : LocalDate = ds match {
        case date : com.trafigura.edm.common.units.Date => date.value
        case _ => throw new Exception("Unsupported DateSpec type " + ds + ", " + ds.getClass)
      }
      (Day.fromLocalDate(getDate(deliverySpec_.schedule)), Day.fromLocalDate(getDate(detail.expectedSales)))
    }
    val quotaQuantity : Quantity = deliverySpec_.quantity
    val contractPricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUID), futuresExchangeByID).fromEdmPricingSpec(contractDeliveryDay, quotaQuantity, detail.pricingSpec)

    val deliveryLocation = {
      val deliveryLocations = deliverySpec_.deliveryLocations
      assert(deliveryLocations.size == 1, "Expect a single delivery location")
      deliveryLocations.head
    }

    val contractLocation = ContractualLocationCode(locationsByGUID(deliveryLocation.location).code)
    val benchmarkCountry = NeptuneCountryCode(destLocationsByGUID(deliverySpec_.destinationLocation).countryCode)

    val contractIncoTerm = IncotermCode(deliveryLocation.incoterm)
    val benchmarkIncoTerm = Option(detail.benchmark).map(IncotermCode)

    val grade = deliverySpec_.materialSpec match {
      case rms: com.trafigura.edm.trademgmt.materialspecification.RefinedMetalSpec => GradeCode(gradeByGUID(rms.grade).code)
      case _ => throw new Exception("Expected RefinedMetalSpec, recieved " + deliverySpec_.materialSpec)
    }

    val isPurchase = trade.direction match {
      case EDMTrade.PURCHASE => true
      case EDMTrade.SALE => false
      case _ => throw new Exception(" Invalid direction " + trade.direction)
    }
    val counterparty = counterpartiesByGUID(trade.counterparty.counterparty).name

    TradeManagementQuotaDetails(
      isPurchase,
      counterparty,
      commodity,
      contractDeliveryDay,
      benchmarkDeliveryDay,
      contractPricingSpec,
      contractLocation,
      benchmarkCountry,
      contractIncoTerm,
      benchmarkIncoTerm,
      grade,
      buildTradeAttributes(refData, trade, detail, eventID)
    )
  }

  def buildTradeAttributes(refData: TitanTacticalRefData, trade : EDMPhysicalTrade, detail : QuotaDetails, eventID : String) : TitanTradeAttributes = {
    import refData._
    val deliverySpec_ = {
      assert(detail.deliverySpecs.size == 1, "Requires there to be a single delivery spec")
      detail.deliverySpecs.head
    }
    val quotaID = NeptuneId(detail.identifier).identifier
    val quotaQuantity : Quantity = deliverySpec_.quantity
    val titanTradeID = NeptuneId(trade.identifier).identifier
    val groupCompany = groupCompaniesByGUID.get(trade.groupCompany).map(_.name).getOrElse("Unknown Group Company")
    val comments = trade.comments
    val contractFinalised = trade.contractFinalised.toString
    val submittedDay = Day.fromLocalDate(trade.submitted.toLocalDate)
    val shape = deliverySpec_.materialSpec match {
      case rms: com.trafigura.edm.trademgmt.materialspecification.RefinedMetalSpec => shapesByGUID(rms.shape).name
      case _ => throw new Exception("Expected RefinedMetalSpec, recieved " + deliverySpec_.materialSpec)
    }
    val (tolerancePlus, toleranceMinus) =  {
      val tolerance = deliverySpec_.tolerance match {
        case tol: PercentageTolerance => tol
        case _ => throw new Exception("Unsupported tolerance")
      }
      def getTolerancePercentage(percentage: Option[Double]): Percentage = percentage match {
        case Some(percentage) => Percentage(percentage * 0.01) // scale since 1% in titan is 1.00 which is 100% in Starling
        case _ => Percentage(0.0)
      }
      (getTolerancePercentage(tolerance.plus.amount), getTolerancePercentage(tolerance.minus.amount))
    }
    TitanTradeAttributes(
      quotaID,
      quotaQuantity,
      titanTradeID,
      groupCompany,
      comments,
      submittedDay,
      shape,
      contractFinalised,
      tolerancePlus,
      toleranceMinus,
      List(eventID)
    )
  }
}

case class InventoryAssignment(assignmentID : String, quotaID : String, quantity : Quantity)

object InventoryAssignment{
  def apply(ass : com.trafigura.edm.logistics.inventory.Assignment) : InventoryAssignment = {
    InventoryAssignment(
      ass.oid.contents.toString,
      ass.quotaTitanId.value.toString,
      ass.quantity
    )
  }
}
object LogisticsStatuses{
  val cancelled = CancelledInventoryItemStatus.toString
  val split = SplitInventoryItemStatus.toString
  val expected = ExpectedInventoryItemStatus.toString
}

case class LogisticsInventory(
  inventoryID : String,
  parentID : Option[String],
  status : String,
  currentQuantity : Quantity,
  purchaseAssignment : InventoryAssignment,
  saleAssignment : Option[InventoryAssignment],
  eventID : String
){
  import LogisticsStatuses._
  def isAllocated = saleAssignment.isDefined
  // if we've reached the receipted inventory status (or later status) we can use the assignment qty, otherwise assignment qty it's not yet valid
  val receivedQuantity: Option[Quantity] = {
    status match {
      case `expected` => None
      case `split` | `cancelled` =>
        throw new Exception("Unexpected inventory status '" + status + "' for inventory item " + inventoryID)
      case _ => Some(purchaseAssignment.quantity)
    }
  }

  // take the receipted qty (if available) to reflect logistics receipt qty, otherwise if not yet received then use the inventory qty as best estimate
  def purchaseAssignmentQuantity: Quantity = receivedQuantity.getOrElse(currentQuantity)
  def quotaIDs = purchaseAssignment.quotaID :: saleAssignment.toList.map(_.quotaID)
}


object LogisticsInventory{
  def apply(item : InventoryItem, eventID : String) : LogisticsInventory = {
    LogisticsInventory(
      item.oid.contents.toString,
      item.parentId.map(_.toString),
      item.status.toString,
      item.quantity,
      InventoryAssignment(item.purchaseAssignment),
      Option(item.salesAssignment).map(InventoryAssignment(_)),
      eventID
    )
  }
}

class PhysicalMetalForwardBuilder(refData: TitanTacticalRefData) extends Log{

  def build(tmqd : TradeManagementQuotaDetails, inventoryItems : List[LogisticsInventory], isFullyAllocated : Boolean) : List[Trade] = {

    import tmqd.attributes
    val QUOTA_PREFIX = "Q-"
    val ASSIGNMENT_PREFIX = "A-"
    def makeAssignment(ass: InventoryAssignment, inv: LogisticsInventory, isPurchase: Boolean) = {

      PhysicalMetalAssignment(
        ass.assignmentID,
        if (isPurchase) inv.purchaseAssignmentQuantity else inv.currentQuantity,
        tmqd.commodity,
        tmqd.contractDeliveryDay,
        tmqd.contractPricingSpec,
        tmqd.contractLocation,
        tmqd.contractIncoTerm,
        if (inv.isAllocated) None else Some(tmqd.benchmarkDeliveryDay),
        if (inv.isAllocated) None else Some(tmqd.benchmarkCountry),
        if (inv.isAllocated) None else tmqd.benchmarkIncoTerm,
        isPurchase,
        inventoryID = inv.inventoryID,
        inventoryQuantity = inv.currentQuantity,
        grade = tmqd.grade
      )
    }

    if (tmqd.isPurchase) {
      val trades = inventoryItems.map {
        inv =>
          val assignment = makeAssignment(inv.purchaseAssignment, inv, true)
          Trade(
            TradeID(ASSIGNMENT_PREFIX + inv.purchaseAssignment.assignmentID, TitanTradeSystem),
            attributes.submitted,
            tmqd.counterparty,
            attributes,
            assignment
          )
      }
      trades
    } else {
      log.info("detail.identifier %s, fullyAllocated %s".format(attributes.quotaID, isFullyAllocated))
      val assignmentTrades = inventoryItems.map {
        inv =>
          val assignment = makeAssignment(inv.saleAssignment.get, inv, false)
          Trade(
            TradeID(ASSIGNMENT_PREFIX + inv.saleAssignment.get.assignmentID, TitanTradeSystem),
            attributes.submitted,
            tmqd.counterparty,
            attributes,
            assignment
          )
      }
      val unassignedSalesQuota = if (isFullyAllocated)
        None
      else {
        val unallocatedQuantity = attributes.quotaQuantity - assignmentTrades.map(_.tradeable.asInstanceOf[PhysicalMetalAssignment].quantity).sum

        log.info("\n****** Unallocated quota %s unallocated qty %s from delivery qty %s - assignmentTrades qty sum %s \n".format(attributes.quotaID, unallocatedQuantity.toString,
          attributes.quotaQuantity.toString, assignmentTrades.map(_.tradeable.asInstanceOf[PhysicalMetalAssignment].quantity).sum.toString))

        val unallocatedQuota = UnallocatedSalesQuota(
          unallocatedQuantity,
          tmqd.commodity,
          tmqd.contractDeliveryDay,
          tmqd.contractPricingSpec,
          tmqd.contractLocation,
          tmqd.contractIncoTerm,
          Some(tmqd.benchmarkDeliveryDay),
          Some(tmqd.benchmarkCountry),
          tmqd.benchmarkIncoTerm,
          tmqd.grade
        )
        Some(Trade(
          TradeID(QUOTA_PREFIX + attributes.quotaID, TitanTradeSystem),
          attributes.submitted,
          tmqd.counterparty,
          attributes,
          unallocatedQuota
        ))
      }
      unassignedSalesQuota.toList ::: assignmentTrades
    }
  }
}

object PhysicalMetalForwardBuilder{
  def apply(trades: List[Trade]) : Either[String, PhysicalMetalForward] = {

    try {
      val allAssignmentsWithAttributes = trades.map {
        trade =>
          (trade.tradeable.asInstanceOf[PhysicalMetalAssignmentOrUnassignedSalesQuota], trade.attributes.asInstanceOf[TitanTradeAttributes])
      }
      val allAssignments = allAssignmentsWithAttributes.map(_._1)
      val allAttributes = allAssignmentsWithAttributes.map(_._2)

      def singleAttribute[B](fn: TitanTradeAttributes => B) = {
        val set = allAttributes.map(fn).toSet
        assert(set.size == 1)
        set.head
      }

      allAssignments match {
        case Nil => throw new Exception("Can not construct Physical Metal Forward from zero trade assignments")
        case _ => {
          val tradeID = singleAttribute(_.titanTradeID)

          val quotas: List[PhysicalMetalQuota] = allAssignmentsWithAttributes.groupBy(a => a._2.quotaID).map {
            case (quotaID, assignmentsWithAttributes) => {
              val assignments = assignmentsWithAttributes.map(_._1)
              val quotaAssignments = assignments.collect {
                case p: PhysicalMetalAssignment => p
              }
              val unallocatedSalesAssignments = assignments.collect {
                case p: UnallocatedSalesQuota => p
              }
              PhysicalMetalQuota(
                quotaID,
                quotaAssignments,
                unallocatedSalesAssignments.headOption
              )
            }
          }.toList

          Right(PhysicalMetalForward(tradeID, quotas))
        }
      }
    } catch {
      case ex => {
        Left(ex.getMessage)
      }
    }
  }
}

