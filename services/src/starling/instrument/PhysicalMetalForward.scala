package starling.instrument

import physical.TitanPricingSpec
import starling.quantity.Quantity
import starling.curves.Environment
import starling.daterange.Day
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market => EDMMarket, Metal => EDMMetal}
import starling.titan.{EDMPricingSpecConverter, EDMConversions}
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota
import java.lang.Exception
import EDMConversions._
import com.trafigura.services.valuation.CostsAndIncomeAssignmentValuation
import com.trafigura.services.valuation.CostsAndIncomeQuotaValuation
import com.trafigura.services.valuation.PricingValuationDetails
import com.trafigura.edm.shared.types.{Date, Quantity => EDMQuantity}
import com.trafigura.edm.logistics.inventory.{EDMAssignment, EDMInventoryItem}


case class PhysicalMetalQuota(
  quotaID : String,
  quantity : Quantity,
  pricingSpec : TitanPricingSpec,
  // For a purchase this is the expected sale, and vice-versa - TODO find a better name
  expectedTransferPricingSpec : TitanPricingSpec) {
}


object PhysicalMetalForward{

  private def getTradeId(t : EDMPhysicalTrade) = {
    if (t.titanId == null) {
      null
    }
    else {
      t.titanId.value
    }
  }

  def apply(exchangesByID : Map[String, EDMMarket], edmMetalByGUID : Map[GUID, EDMMetal])(trade : EDMPhysicalTrade) : PhysicalMetalForward = {
    try {
      val quotas : List[PhysicalMetalQuota] = {
        trade.quotas.map(_.detail).map {
          detail =>
            val deliveryQuantity = detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
            val commodityGUIDs : Set[GUID] = detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet

            assert(commodityGUIDs.size == 1, "Trade " + getTradeId(trade) + " has multiple commodities")
            val deliveryDay = Day.fromLocalDate(detail.deliverySpecs.head.schedule.asInstanceOf[Date].value)

            val pricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, detail.pricingSpec)
            PhysicalMetalQuota(
              detail.identifier.value,
              deliveryQuantity,
              pricingSpec,
              pricingSpec.dummyTransferPricingSpec
            )
        }
      }

      val isPurchase = trade.direction match {
        case EDMTrade.PURCHASE => true
        case EDMTrade.SALE => false
        case _ => throw new Exception("Trade " + getTradeId(trade) + " has no direction " + trade.direction)
      }
      PhysicalMetalForward(getTradeId(trade), quotas, isPurchase)
    }
    catch {
      case ex => throw new Exception("Trade " + getTradeId(trade) + " failed to construct from EDM. " + ex.getMessage, ex)
    }
  }

  def value(exchangesByID : Map[String, EDMMarket], edmMetalByGUID : Map[GUID, EDMMetal], env : Environment, snapshotID : String)(trade : EDMPhysicalTrade) : Either[String, List[CostsAndIncomeQuotaValuation]] =  {

    try {
      val forward = PhysicalMetalForward(exchangesByID, edmMetalByGUID)(trade)
      Right(forward.costsAndIncomeValueBreakdown(env, snapshotID))
    }
    catch {
      case ex => Left("Error valuing trade " + getTradeId(trade) + ", message was " + ex.getMessage)
    }
  }
}

case class PhysicalMetalForward(tradeID : String, quotas : List[PhysicalMetalQuota], isPurchase : Boolean) {

  def costsAndIncomeValueBreakdown(env: Environment, snapshotID: String): List[CostsAndIncomeQuotaValuation] = {
    quotas.map {
      quota =>
        val pricingSpec = quota.pricingSpec
        val transferPricingSpec = quota.expectedTransferPricingSpec
        val (purchaseSpec, saleSpec) =
          if (isPurchase)
            (pricingSpec, transferPricingSpec)
          else
            (transferPricingSpec, pricingSpec)

        CostsAndIncomeQuotaValuation(
          quota.quotaID,
          snapshotID,
          quota.quantity,
          direction = if (isPurchase) EDMTrade.PURCHASE else EDMTrade.SALE,
          benchmarkdirection = if (isPurchase) Some(EDMTrade.SALE) else Some(EDMTrade.PURCHASE),
          purchaseValuationDetails = CostsAndIncomeValuation.build(env, quota.quantity, purchaseSpec),
          saleValuationDetails = CostsAndIncomeValuation.build(env, quota.quantity, saleSpec),
          benchmarkPremium = Quantity.NULL,
          freightParity = Quantity.NULL
        )
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
      pricingSpec.quotationPeriodStart.map(_.toJodaLocalDate),
      pricingSpec.quotationPeriodEnd.map(_.toJodaLocalDate),
      pricingSpec.indexName
    )
  }
}

object PhysicalMetalAssignmentForward {
  def apply(exchangesByID : Map[String, EDMMarket],
            edmMetalByGUID : Map[GUID, EDMMetal],
            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota],
            uomIdToNameMap : Map[Int, String])
           (inventory : EDMInventoryItem) : PhysicalMetalAssignmentForward = {
    try {
      val purchaseQuota = quotaNameToQuotaMap(inventory.purchaseAssignment.quotaName)
      val edmPurchasePricingSpec = purchaseQuota.detail.pricingSpec
      val edmSalePricingSpec = inventory.salesAssignment match {
        case null => edmPurchasePricingSpec
        case salesAssignment : EDMAssignment => quotaNameToQuotaMap(salesAssignment.quotaName).detail.pricingSpec
      }

      val deliveryQuantity = purchaseQuota.detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
      val commodityGUIDs : Set[GUID] = purchaseQuota.detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet
      assert(commodityGUIDs.size == 1, "quota " + purchaseQuota.detail.identifier + " has multiple commodities")
      val deliveryDay = Day.fromLocalDate(purchaseQuota.detail.deliverySpecs.head.schedule.asInstanceOf[Date].value)

      val purchasePricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmPurchasePricingSpec)
      val salePricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmSalePricingSpec)
            
     val quota = PhysicalMetalQuota(
       purchaseQuota.detail.identifier.value,
       deliveryQuantity,
       purchasePricingSpec,
       salePricingSpec)

      PhysicalMetalAssignmentForward(inventory.oid.contents.toString, inventory, quota, uomIdToNameMap)
    }
    catch {
      case ex => throw new Exception("Inventory " + inventory.oid + " failed to construct from EDM. " + ex.getMessage, ex)
    }
  }

  def value(exchangesByID : Map[String, EDMMarket],
            edmMetalByGUID : Map[GUID, EDMMetal],
            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota],
            uomIdToNameMap : Map[Int, String],
            env : Environment, snapshotID : String)
            (inventory : EDMInventoryItem) : Either[String, List[CostsAndIncomeAssignmentValuation]] =  {

    try {
      val forward = PhysicalMetalAssignmentForward(exchangesByID, edmMetalByGUID, quotaNameToQuotaMap, uomIdToNameMap)(inventory)
      Right(forward.costsAndIncomeAssignmentValueBreakdown(env, snapshotID))
    }
    catch {
      case ex => Left("Error valuing inventory  " + inventory.oid + ", message was " + ex.getMessage)
    }
  }
}

case class PhysicalMetalAssignmentForward(id : String, inventoryItem : EDMInventoryItem, quota : PhysicalMetalQuota, uomIdToName : Map[Int, String]) {

  def costsAndIncomeAssignmentValueBreakdown(env : Environment, snapshotId : String) : List[CostsAndIncomeAssignmentValuation] = {
    val purchaseQty = fromTitanQuantity(inventoryItem.quantity, uomIdToName)
    val saleQty = purchaseQty // until logistics send us both quantities

    List(CostsAndIncomeAssignmentValuation(
      purchaseAssignmentID = inventoryItem.purchaseAssignment.oid.contents.toString,
      saleAssignmentID = inventoryItem.salesAssignment match { case null => None; case salesAssignment : EDMAssignment => Some(salesAssignment.oid.contents.toString) },
      snapshotID = snapshotId,
      purchaseQuantity = toTitanSerializableQuantity(purchaseQty),
      saleQuantity = toTitanSerializableQuantity(saleQty),

      // These come from the pricing spec of the quota associated with the purchaseAssignmentID
      purchaseValuationDetails = CostsAndIncomeValuation.build(env, purchaseQty, quota.pricingSpec),
      // If saleAssignmentID is defined, then these details come from the pricing spec of its associated quota,
      // otherwise use the expected pricing spec of the purchase quota
      saleValuationDetails = CostsAndIncomeValuation.build(env, saleQty, quota.expectedTransferPricingSpec),

      benchmarkPremium = Quantity.NULL,
      freightParity = Quantity.NULL))
  }
}

