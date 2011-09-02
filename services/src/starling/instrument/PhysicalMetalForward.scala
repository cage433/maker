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
import starling.utils.Log


case class PhysicalMetalQuota(
  quotaID : String,
  quantity : Quantity,
  pricingSpec : TitanPricingSpec,
  // For a purchase this is the expected sale, and vice-versa - TODO find a better name
  expectedTransferPricingSpec : TitanPricingSpec) {
}


object PhysicalMetalForward extends Log {

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
      case ex => {
        log.debug("Failed to construct PhysicalMetalForward from EDM ", ex) //.getStackTrace.map(_.toString).mkString(",\n"))
        throw new Exception("Trade with id " + getTradeId(trade) + " failed to construct from EDM. " + ex.getMessage, ex)
      }
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
          snapshotID,
          purchaseValuationDetails = CostsAndIncomeValuation.build(env, quota.quantity, purchaseSpec),
          saleValuationDetails = CostsAndIncomeValuation.build(env, quota.quantity, saleSpec),
          benchmarkPremium = Quantity.NULL,
          freightParity = Quantity.NULL,
          quotaID = quota.quotaID,
          quantity = quota.quantity,
          direction = if (isPurchase) EDMTrade.PURCHASE else EDMTrade.SALE,
          benchmarkdirection = if (isPurchase) Some(EDMTrade.SALE) else Some(EDMTrade.PURCHASE)
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
      pricingSpec.quotationPeriod.map(_.firstDay.toJodaLocalDate),
      pricingSpec.quotationPeriod.map(_.lastDay.toJodaLocalDate),
      pricingSpec.indexOption.map(_.toString).getOrElse("Undefined")
    )
  }
}

object PhysicalMetalAssignmentForward extends Log {
  def apply(exchangesByID : Map[String, EDMMarket],
            edmMetalByGUID : Map[GUID, EDMMetal],
            quotaNameToQuotaMap : Map[String, PhysicalTradeQuota])
            (inventory : EDMInventoryItem)
            (implicit uomIdToNameMap : Map[Int, String]) : PhysicalMetalAssignmentForward = {
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
       purchaseQuota.detail.identifier.value,
       deliveryQuantity,
       purchasePricingSpec,
       salePricingSpec)

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
            (inventory : EDMInventoryItem)
            (implicit uomIdToNameMap : Map[Int, String]): Either[String, List[CostsAndIncomeAssignmentValuation]] =  {

    try {
      val forward = PhysicalMetalAssignmentForward(exchangesByID, edmMetalByGUID, quotaNameToQuotaMap)(inventory)
      Right(forward.costsAndIncomeAssignmentValueBreakdown(env, snapshotID))
    }
    catch {
      case ex => Left("Error valuing inventory  " + inventory.oid.contents + ", purchase assignment " + inventory.purchaseAssignment.oid.contents + ", sales assignment " + Option(inventory.salesAssignment).map(_.oid.contents.toString).getOrElse("No sales") + ", message was " + ex.getMessage)
    }
  }
}

case class PhysicalMetalAssignmentForward(id : String, inventoryItem : EDMInventoryItem, quota : PhysicalMetalQuota)(implicit uomIdToName : Map[Int, String]) {

  def costsAndIncomeAssignmentValueBreakdown(env : Environment, snapshotId : String) : List[CostsAndIncomeAssignmentValuation] = {
    val purchaseQty = fromTitanQuantity(inventoryItem.purchaseAssignment.quantity)

    // would prefer the actual sales assignment quantity when known, otherwise fall back to the inventory quantity
    //   (which may differ from the purchase assignment quantity over time)
    val saleQty = Option(inventoryItem.salesAssignment) match {
      case Some(salesAssignment) => salesAssignment.quantity
      case _ => inventoryItem.quantity
    }

    List(CostsAndIncomeAssignmentValuation(
      snapshotID = snapshotId,
      // These come from the pricing spec of the quota associated with the purchaseAssignmentID
      purchaseValuationDetails = CostsAndIncomeValuation.build(env, purchaseQty, quota.pricingSpec),
      // If saleAssignmentID is defined, then these details come from the pricing spec of its associated quota,
      // otherwise use the expected pricing spec of the purchase quota
      saleValuationDetails = CostsAndIncomeValuation.build(env, saleQty, quota.expectedTransferPricingSpec),
      benchmarkPremium = Quantity.NULL,
      freightParity = Quantity.NULL,
      purchaseAssignmentID = inventoryItem.purchaseAssignment.oid.contents.toString,
      saleAssignmentID = inventoryItem.salesAssignment match { case null => None; case salesAssignment : EDMAssignment => Some(salesAssignment.oid.contents.toString) },
      purchaseQuantity = toTitanSerializableQuantity(purchaseQty),
      saleQuantity = toTitanSerializableQuantity(saleQty))
    )
  }
}

