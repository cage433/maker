package starling.instrument

import physical.TitanPricingSpec
import starling.quantity.Quantity
import starling.curves.Environment
import starling.daterange.Day
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market => EDMMarket, Metal => EDMMetal}
import starling.titan.{EDMPricingSpecConverter, EDMConversions}
import starling.titan.EDMConversions._
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota
import java.lang.Exception
import EDMConversions._
import starling.utils.Log
import com.trafigura.edm.shared.types.{TitanId, Date, Quantity => EDMQuantity}
import com.trafigura.edm.logistics.inventory._
import com.trafigura.services.valuation._
import com.trafigura.services.TitanSerializableQuantity


/**
 * Represents a logics inventory (+ associated assignments) and the rules around how to get quanties depending on state and direction
 */
case class Inventory(item : EDMInventoryItem) {

  val receivedQuantity : Option[Quantity] = {
    item.status match {
      case ExpectedEDMInventoryItemStatus => None
      case SplitEDMInventoryItemStatus | CancelledEDMInventoryItemStatus => throw new Exception("Unexpected inventory status " + item.status)
      case _ => Some(item.purchaseAssignment.quantity)
    }
  }

  def assignmentQuantity : Quantity = {
    receivedQuantity.getOrElse(item.quantity)
  }

  def isAllocated = Option(item.salesAssignment).isDefined

  def currentQuantity = item.quantity
}

case class PhysicalMetalAssignment(assignment : EDMAssignment, inventory : Inventory, pricingSpec : TitanPricingSpec, benchmarkPricingSpec : TitanPricingSpec){
  val assignmentQuantity: Quantity = inventory.assignmentQuantity
  val isPurchase = PhysicalMetalForward.isPurchase(assignment.direction)
  val assignmentID: String = assignment.oid.contents.toString
  val weightGain: Quantity = inventory.currentQuantity - assignmentQuantity

  val isAllocated: Boolean = inventory.isAllocated
  
  def allocatedPurchaseValue(env : Environment) : CostsAndIncomeAllocatedPurchaseAssignmentValuation = {
    require(isAllocated && isPurchase)
    CostsAndIncomeAllocatedPurchaseAssignmentValuation(
      assignmentID,
      /*
         Rows 71, 72, 74
         For a purchase assignment this is the final receipted quantity (when it exists) otherwise current inventory quantity
         For a salee assignment this is the final delivered quantity (when it exists) otherwise current inventory quantity
       */
      assignmentQuantity,
      CostsAndIncomeValuation.buildEither(env, assignmentQuantity, pricingSpec)
    )
  }

  def unallocatedPurchaseValue(env : Environment) : CostsAndIncomeUnallocatedAssignmentValuation = {
    require (!isAllocated && isPurchase)
    CostsAndIncomeUnallocatedAssignmentValuation(
      assignmentID,
      /*
        Row 71
        Defined using the same rule as CostsAndIncomeAllocatedAssignmentValuation, however note that unallocated
        assignments only exist on the purchase side.
       */
      assignmentQuantity,
      CostsAndIncomeValuation.buildEither(env, assignmentQuantity, pricingSpec),
      CostsAndIncomeValuation.buildEither(env, assignmentQuantity, benchmarkPricingSpec),
      /*
        Rows 73, 75
        current inventory quantity - assignmentQuantity
       */
      weightGain,
      CostsAndIncomeValuation.buildEither(env, weightGain, benchmarkPricingSpec),
      freightParity = Quantity.NULL
    )
  }

  
  def allocatedSaleValue(env : Environment) : CostsAndIncomeAllocatedSaleAssignmentValuation = {
    require (isAllocated && !isPurchase)
    CostsAndIncomeAllocatedSaleAssignmentValuation(
      assignmentID,
      /*
         Rows 71, 72, 74
         For a purchase assignment this is the final receipted quantity (when it exists) otherwise current inventory quantity
         For a salee assignment this is the final delivered quantity (when it exists) otherwise current inventory quantity
       */
      assignmentQuantity,
      CostsAndIncomeValuation.buildEither(env, assignmentQuantity, pricingSpec),
      weightGain,
      CostsAndIncomeValuation.buildEither(env, weightGain, pricingSpec)
    )
  }
}

/**
 * represent a physical metal quota as required for valuations,
 *   contains data from trademgmt (edm trade service) and logistics (quota) services
 */
case class PhysicalMetalQuota(
  isPurchase : Boolean,
  quotaID : String,
  quantity : Quantity,
  contractPricingSpec : TitanPricingSpec,
  // For a purchase this is the expected sale, and vice-versa
  benchmarkPricingSpec : TitanPricingSpec,

  // Logistics sourced quota status:
  //   true indicated the quota is fully allocated and has no quota level valuation components (All at the assignment level)
  //   false indicates the quota has all valuation at the assignment level or some split of assignment and unallocated quota level valuations
  fullyAllocated : Boolean = false,
  inventory : List[Inventory] = Nil) {

  // check - fullyAllocated = true and 0 quantity = errror state
  require(!(fullyAllocated && allocatedAssignmentQuantity == 0))  // this check may not be valid according to the sales quota allocation status scenario grid (S-11822)

  // the unallocated quota quantity
  def unallocatedQuotaQuantity : Quantity = {
    /**
     * work out the quota level quantities
     *   from the direction and rules around allocated quantities and fully allocated status
     */
    if (isPurchase) {
      unallocatedAssignments.map{a => fromTitanQuantity(a.assignmentQuantity)}.sum
    } else {
        if (!fullyAllocated)
          quantity - allocatedAssignmentQuantity
        else
          quantity * 0 // quota level quantity is zero when fully allocated on the sales side
    }
  }

  // using actual assignment level quantities
  def allocatedAssignmentQuantity : Quantity = {
    allocatedAssignments.map{a => fromTitanQuantity(a.assignmentQuantity)}.sum
  }

  private def makeAssignment(a : EDMAssignment, i : Inventory) = PhysicalMetalAssignment(a, i, contractPricingSpec, benchmarkPricingSpec)
  private def allocatedAssignments : List[PhysicalMetalAssignment] = {
    if (isPurchase) {
      // return purchase assignments that have been allocated to a corresponding sales assignment via the assigned inventory
      inventory.filter(i => Option(i.item.salesAssignment).isDefined).map{i => makeAssignment(i.item.purchaseAssignment, i)}
    }
    else
      // return the sales assignments (all)
      inventory.map{i =>
        makeAssignment(i.item.salesAssignment, i)
      }
  }

  private def unallocatedAssignments : List[PhysicalMetalAssignment] = {
    if (isPurchase)
      inventory.filterNot(i => Option(i.item.salesAssignment).isDefined).map{i => makeAssignment(i.item.purchaseAssignment, i)}
    else
      Nil
  }
  private implicit def toValuationMap[A <: AssignmentValuation](values : List[A]) : Map[String, A] = values.map{v => v.assignmentID -> v}.toMap

  def value(env : Environment) : QuotaValuation = {
    if (isPurchase)
        PurchaseQuotaValuation(
          quotaID,
          /*
            Sum of unallocated AND allocated assignment quantities - Row 64
           */
         allocatedAssignmentQuantity + unallocatedQuotaQuantity,
          /*
            Sum of unallocated assignment quantities - Row 65
           */
         unallocatedQuantity = unallocatedQuotaQuantity,
         /*
           Sum of unallocated weight gain/loss quantities - Row 66
           Once purchase assignments are allocated the wight gain/loss moves to the sales assignment
         */
         weightGainOrLoss = unallocatedAssignments.map(_.weightGain).sum,
         unallocatedValuations = unallocatedAssignments.map{ass => ass.unallocatedPurchaseValue(env)},
         assignmentValuations = allocatedAssignments.map{ ass => ass.allocatedPurchaseValue(env)}
      )
    else
        SalesQuotaValuation(
          quotaID,
          fullyAllocated,
          /*
            Row 67 and Row 70
            Contactual quantity - allocated quantity when not fully allocated, otherwise 0
           */
          unallocatedQuantity = unallocatedQuotaQuantity,
          unallocatedValuationDetails = CostsAndIncomeValuation.buildEither(env, unallocatedQuotaQuantity, contractPricingSpec), 
          benchmarkDetails = CostsAndIncomeValuation.buildEither(env, unallocatedQuotaQuantity, benchmarkPricingSpec),
          /*
            Row 68
            Sum of allocated assignment quantities
           */
          allocatedQuantity = allocatedAssignmentQuantity,
          assignmentValuations = allocatedAssignments.map{ ass => ass.allocatedSaleValue(env)},

          /*
            Row 69
            For unallocated assignments, sum og weight gain/loss
           */
          weightGainOrLoss = allocatedAssignments.map(_.weightGain).sum
        )
  }
}


//case class InventoryQuantities(inventoryID : String, received : Option[Quantity], delivered : Option[Quantity], current : Quantity){
//  def purchaseAssignmentQuantity = received.getOrElse(current)
//}


object PhysicalMetalForward extends Log {

  private def getTradeId(t : EDMPhysicalTrade) : String = Option(t.titanId).map(_.value).getOrElse("<null>")

  // **********
  // very temporary, until we merge from master (with logistics updates)
  // **********
  trait EDMLogisticsQuota {
    val fullyAllocated : Boolean
  }

  def isPurchase(direction : String): Boolean = {
    val isPurchase = direction match {
      case EDMTrade.PURCHASE => true
      case EDMTrade.SALE => false
      case _ => throw new Exception(" Invalid direction " + direction)
    }
    isPurchase
  }

  def apply(
            exchangesByID : Map[String, EDMMarket],
            edmMetalByGUID : Map[GUID, EDMMetal],
            inventoryByQuotaID : Map[TitanId, List[EDMInventoryItem]] = Map(),
            logisticsQuotaByQuotaID : Map[TitanId, EDMLogisticsQuota] = Map())
            (trade : EDMPhysicalTrade) : PhysicalMetalForward = {

    try {
      val isPurchaseTrade: Boolean = isPurchase(trade.direction)

      val quotas : List[PhysicalMetalQuota] = {
        trade.quotas.map(_.detail).map {
          detail =>
            val deliveryQuantity = detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
            val commodityGUIDs : Set[GUID] = detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet

            assert(commodityGUIDs.size == 1, "Trade " + getTradeId(trade) + " has multiple commodities")
            val deliveryDay = Day.fromLocalDate(detail.deliverySpecs.head.schedule.asInstanceOf[Date].value)

            val pricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, detail.pricingSpec)
            

            /**
             * Fetch the associated logistics inventory and associated logistics quota so that we can retrieve the fields relevant to the valuation process
             *   which include the fullyAllocated (bool) and the allocatedQuantity (quantity) fields from the associated logistics quota
             */
            val inventoryItems = inventoryByQuotaID.get(detail.identifier).flatten.toList.map(i => Inventory(i))
            if (isPurchaseTrade)
              assert(inventoryItems.size > 0, "Purchase quota %s with no logistics inventory".format(detail.identifier.value))
            
            //def getLogisticsQuota(quotaName : String) : Option[EDMLogisticsQuota] = inventory.associatedQuota.find(_.quotaName == detail.quotaName)
            //val logisticsQuota : Option[EDMLogisticsQuota] = getLogisticsQuota(details.quotaName)
            
            val logisticsQuota : Option[EDMLogisticsQuota] = logisticsQuotaByQuotaID.get(detail.identifier)
            
            val isFullyAllocated = logisticsQuota.map(_.fullyAllocated).getOrElse(false)


            PhysicalMetalQuota(
              isPurchaseTrade,
              detail.identifier.value,
              deliveryQuantity,
              pricingSpec,
              pricingSpec.dummyTransferPricingSpec,
              isFullyAllocated,
              inventoryItems
            )
        }
      }

      PhysicalMetalForward(getTradeId(trade), quotas, isPurchaseTrade)
    }
    catch {
      case ex => {
        log.debug("Failed to construct PhysicalMetalForward from EDM ", ex) //.getStackTrace.map(_.toString).mkString(",\n"))
        throw new Exception("Trade with id " + getTradeId(trade) + " failed to construct from EDM. " + ex.getMessage, ex)
      }
    }
  }

  /**
   * value full quota with associated assignments/inventory and adjustments for allocated quantities at the quota level
   */
  def valueWithAssignments(exchangesByID : Map[String, EDMMarket], edmMetalByGUID : Map[GUID, EDMMetal], env : Environment, snapshotID : String)(trade : EDMPhysicalTrade) : Either[String, List[QuotaValuation]] = {
    try {
      val forward = PhysicalMetalForward(exchangesByID, edmMetalByGUID)(trade)
      Right(forward.costsAndIncomeQuotaValueBreakdown(env, snapshotID))
    }
    catch {
      case ex => {
        Log.warn("Error valuing trade " + getTradeId(trade) + ", message was " + ex.getMessage)
        Left("Error valuing trade " + getTradeId(trade) + ", message was " + ex.getMessage)
      }
    }
  }

}

/**
 * represents a physical metal forward position
 */
case class PhysicalMetalForward(tradeID : String, quotas : List[PhysicalMetalQuota], isPurchase : Boolean) {

  /**
   * value trade quotas with their associated assignment values and adjusted quota portion values
   */
  def costsAndIncomeQuotaValueBreakdown(env: Environment, snapshotID: String) : List[QuotaValuation] = {
    quotas.map {
      quota =>
        quota.value(env)
    }
  }
}

object CostsAndIncomeValuation{
  def build(env : Environment, quantity : Quantity, pricingSpec : TitanPricingSpec) = {
    PricingValuationDetails(
      pricingSpec.price(env),
      pricingSpec.premium,
      pricingSpec.price(env) * quantity,
      pricingSpec.isComplete(env.marketDay),
      pricingSpec.fixedQuantity(env.marketDay, quantity),
      pricingSpec.pricingType,
      pricingSpec.quotationPeriod.map(_.firstDay.toJodaLocalDate),
      pricingSpec.quotationPeriod.map(_.lastDay.toJodaLocalDate),
      pricingSpec.indexOption.map(_.toString).getOrElse("Undefined")
    )
  }

  def buildEither(env : Environment, quantity : Quantity, pricingSpec : TitanPricingSpec) = {
    try {
      Right(build(env, quantity, pricingSpec))
    } catch {
      case e =>
        Left(e.getMessage)
    }
  }

}

object PhysicalMetalAssignmentForward extends Log {
  def apply(exchangesByID : Map[String, EDMMarket],
            edmMetalByGUID : Map[GUID, EDMMetal],
            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota])
            (inventory : EDMInventoryItem) : PhysicalMetalAssignmentForward = {
    try {
      val quotaMap : Map[String, PhysicalTradeQuota] = quotaNameToQuotaMap.withDefault(k => throw new Exception("Missing key '%s' for quota lookup".format(k)))
      val purchaseQuota = quotaMap(inventory.purchaseAssignment.quotaName)
      val edmPurchasePricingSpec = purchaseQuota.detail.pricingSpec
      val edmSalePricingSpec = inventory.salesAssignment match {
        case null => edmPurchasePricingSpec
        case salesAssignment : EDMAssignment => {
          quotaMap(salesAssignment.quotaName).detail.pricingSpec
        }
      }

      val deliveryQuantity = purchaseQuota.detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
      val commodityGUIDs : Set[GUID] = purchaseQuota.detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet
      assert(commodityGUIDs.size == 1, "quota " + purchaseQuota.detail.identifier + " has multiple commodities")

      val deliveryDay = Day.fromLocalDate(purchaseQuota.detail.deliverySpecs.head.schedule.asInstanceOf[Date].value)
      val purchasePricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmPurchasePricingSpec)
      val salePricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmSalePricingSpec)
            
     val quota = PhysicalMetalQuota(
       true, // place-holder, always defined from the purchase side pov, possibly the PhysicalMetalQuota will get refactored to not be a composite of purchase and sale
       purchaseQuota.detail.identifier.value,
       deliveryQuantity,
       purchasePricingSpec,
       salePricingSpec,
      false,
      List(Inventory(inventory)))

      PhysicalMetalAssignmentForward(inventory.oid.contents.toString, inventory, quota)
    }
    catch {
      case ex => {
        log.debug("Failed to construct PhysicalMetalAssignmentForward from EDM ", ex)
        throw new Exception("Inventory with id " + inventory.oid.contents + " failed to construct from EDM. " + ex.getMessage, ex)
      }
    }
  }

  def value(exchangesByID : Map[String, EDMMarket],
            edmMetalByGUID : Map[GUID, EDMMetal],
            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota],
            env : Environment, snapshotID : String)
            (inventory : EDMInventoryItem) : Either[String, List[CostsAndIncomeInventoryValuation]] =  {

    try {
      val forward = PhysicalMetalAssignmentForward(exchangesByID, edmMetalByGUID, quotaNameToQuotaMap)(inventory)
      Right(forward.costsAndIncomeAssignmentValueBreakdown(env, snapshotID))
    }
    catch {
      case ex => Left("Error valuing inventory  " + inventory.oid.contents + ", purchase assignment " + inventory.purchaseAssignment.oid.contents + ", sales assignment " + Option(inventory.salesAssignment).map(_.oid.contents.toString).getOrElse("No sales") + ", message was " + ex.getMessage)
    }
  }
}

case class PhysicalMetalAssignmentForward(id : String, inventoryItem : EDMInventoryItem, quota : PhysicalMetalQuota) {

  def costsAndIncomeAssignmentValueBreakdown(env : Environment, snapshotId : String) : List[CostsAndIncomeInventoryValuation] = {
    val purchaseQty = fromTitanQuantity(inventoryItem.purchaseAssignment.quantity)

    // would prefer the actual sales assignment quantity when known, otherwise fall back to the inventory quantity
    //   (which may differ from the purchase assignment quantity over time)
    val saleQty = Option(inventoryItem.salesAssignment) match {
      case Some(salesAssignment) => salesAssignment.quantity
      case _ => inventoryItem.quantity
    }

    List(CostsAndIncomeInventoryValuation(
      snapshotID = snapshotId,
      // These come from the pricing spec of the quota associated with the purchaseAssignmentID
      purchaseValuationDetails = CostsAndIncomeValuation.build(env, purchaseQty, quota.contractPricingSpec),
      // If saleAssignmentID is defined, then these details come from the pricing spec of its associated quota,
      // otherwise use the expected pricing spec of the purchase quota
      saleValuationDetails = CostsAndIncomeValuation.build(env, saleQty, quota.benchmarkPricingSpec),
      benchmarkPremium = Quantity.NULL,
      freightParity = Quantity.NULL,
      purchaseAssignmentID = inventoryItem.purchaseAssignment.oid.contents.toString,
      saleAssignmentID = inventoryItem.salesAssignment match { case null => None; case salesAssignment : EDMAssignment => Some(salesAssignment.oid.contents.toString) },
      purchaseQuantity = toTitanSerializableQuantity(purchaseQty),
      saleQuantity = toTitanSerializableQuantity(saleQty))
    )
  }
}
