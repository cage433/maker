package starling.instrument

import physical._
import starling.daterange.Day
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market => EDMMarket, Metal => EDMMetal}
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import java.lang.Exception
import starling.titan.{TitanTacticalRefData, EDMPricingSpecConverter}
import starling.utils.Log
import com.trafigura.edm.logistics.inventory._
import starling.quantity.{Percentage, Quantity}
import com.trafigura.edm.shared.types.{DateSpec, PercentageTolerance, TitanId, Date, Quantity => EDMQuantity}
import org.joda.time.LocalDate
import starling.titan.EDMConversions._
import com.trafigura.edm.physicaltradespecs.{DeliverySpec, QuotaDetails}

/**
 * Represents a logics inventory (+ associated assignments) and the rules around how to get quanties depending on state and direction
 */

//object PhysicalMetalAssignmentBuilder{
//  def apply(assignment : EDMAssignment, inventory : Inventory, pricingSpec : TitanPricingSpec, benchmarkPricingSpec : TitanPricingSpec) : PhysicalMetalAssignment = {
//    val assignmentQuantity: Quantity = inventory.assignmentQuantity
//    val isPurchase = PhysicalMetalForward.isPurchase(assignment.direction)
//    val assignmentID: String = assignment.oid.contents.toString
//    val inventoryQuantity = inventory.currentQuantity
//
//    val isAllocated: Boolean = inventory.isAllocated
//
//    PhysicalMetalAssignment(
//     commodityName,
//     assignmentQuantity,
//     deliveryDay,
//     pricingSpec,
//     benchmarkPricingSpec,
//     isAllocated,
//     isPurchase,
//     inventoryQuantity
//    )
//  }
//}
//case class PhysicalMetalAssignment(assignment : EDMAssignment, inventory : Inventory, pricingSpec : TitanPricingSpec, benchmarkPricingSpec : TitanPricingSpec){




//object PhysicalMetalAssignmentForward extends Log {
//  def apply(exchangesByID : Map[String, EDMMarket],
//            edmMetalByGUID : Map[GUID, EDMMetal],
//            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota])
//            (inventory : EDMInventoryItem) : PhysicalMetalAssignmentForward = {
//    try {
//      val quotaMap : Map[String, PhysicalTradeQuota] = quotaNameToQuotaMap.withDefault(k => throw new Exception("Missing key '%s' for quota lookup".format(k)))
//      val purchaseQuota = quotaMap(inventory.purchaseAssignment.quotaName)
//      val edmPurchasePricingSpec = purchaseQuota.detail.pricingSpec
//      val edmSalePricingSpec = inventory.salesAssignment match {
//        case null => edmPurchasePricingSpec
//        case salesAssignment : EDMAssignment => {
//          quotaMap(salesAssignment.quotaName).detail.pricingSpec
//        }
//      }
//
//      val deliveryQuantity = purchaseQuota.detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
//      val commodityGUIDs : Set[GUID] = purchaseQuota.detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet
//      assert(commodityGUIDs.size == 1, "quota " + purchaseQuota.detail.identifier + " has multiple commodities")
//
//      val deliveryDay = Day.fromLocalDate(purchaseQuota.detail.deliverySpecs.head.schedule.asInstanceOf[Date].value)
//      val purchasePricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmPurchasePricingSpec)
//      val salePricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmSalePricingSpec)
//
//     val quota = PhysicalMetalQuota(
//       true, // place-holder, always defined from the purchase side pov, possibly the PhysicalMetalQuota will get refactored to not be a composite of purchase and sale
//       purchaseQuota.detail.identifier.value,
//       deliveryQuantity,
//       purchasePricingSpec,
//       salePricingSpec,
//      false,
//      List(Inventory(inventory)))
//
//      PhysicalMetalAssignmentForward(inventory.oid.contents.toString, inventory, quota)
//    }
//    catch {
//      case ex => {
//        log.debug("Failed to construct PhysicalMetalAssignmentForward from EDM ", ex)
//        throw new Exception("Inventory with id " + inventory.oid.contents + " failed to construct from EDM. " + ex.getMessage, ex)
//      }
//    }
//  }
//
//  def value(exchangesByID : Map[String, EDMMarket],
//            edmMetalByGUID : Map[GUID, EDMMetal],
//            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota],
//            env : Environment, snapshotID : String)
//            (inventory : EDMInventoryItem) : Either[String, List[CostsAndIncomeInventoryValuation]] =  {
//
//    try {
//      val forward = PhysicalMetalAssignmentForward(exchangesByID, edmMetalByGUID, quotaNameToQuotaMap)(inventory)
//      Right(forward.costsAndIncomeAssignmentValueBreakdown(env, snapshotID))
//    }
//    catch {
//      case ex => Left("Error valuing inventory  " + inventory.oid.contents + ", purchase assignment " + inventory.purchaseAssignment.oid.contents + ", sales assignment " + Option(inventory.salesAssignment).map(_.oid.contents.toString).getOrElse("No sales") + ", message was " + ex.getMessage)
//    }
//  }
//}

//case class PhysicalMetalAssignmentForward(id : String, inventoryItem : EDMInventoryItem, quota : PhysicalMetalQuota) {
//
//  def costsAndIncomeAssignmentValueBreakdown(env : Environment, snapshotId : String) : List[CostsAndIncomeInventoryValuation] = {
//    val purchaseQty = fromTitanQuantity(inventoryItem.purchaseAssignment.quantity)
//
//    // would prefer the actual sales assignment quantity when known, otherwise fall back to the inventory quantity
//    //   (which may differ from the purchase assignment quantity over time)
//    val saleQty = Option(inventoryItem.salesAssignment) match {
//      case Some(salesAssignment) => salesAssignment.quantity
//      case _ => inventoryItem.quantity
//    }
//
//    List(CostsAndIncomeInventoryValuation(
//      snapshotID = snapshotId,
//      // These come from the pricing spec of the quota associated with the purchaseAssignmentID
//      purchaseValuationDetails = CostsAndIncomeValuation.build(env, purchaseQty, quota.contractPricingSpec),
//      // If saleAssignmentID is defined, then these details come from the pricing spec of its associated quota,
//      // otherwise use the expected pricing spec of the purchase quota
//      saleValuationDetails = CostsAndIncomeValuation.build(env, saleQty, quota.benchmarkPricingSpec),
//      benchmarkPremium = Quantity.NULL,
//      freightParity = Quantity.NULL,
//      purchaseAssignmentID = inventoryItem.purchaseAssignment.oid.contents.toString,
//      saleAssignmentID = inventoryItem.salesAssignment match { case null => None; case salesAssignment : EDMAssignment => Some(salesAssignment.oid.contents.toString) },
//      purchaseQuantity = toTitanSerializableQuantity(purchaseQty),
//      saleQuantity = toTitanSerializableQuantity(saleQty))
//    )
//  }
//}
