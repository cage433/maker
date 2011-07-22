package starling.instrument

import physical.TitanPricingSpec
import starling.quantity.Quantity
import starling.curves.Environment
import starling.market.SingleIndex
import starling.daterange.{DayOfWeek, Month, Day}
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market => EDMMarket, Metal => EDMMetal}
import starling.titan.{EDMPricingSpecConverter, EDMConversions}
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.physicaltradespecs.EDMQuota
import java.lang.Exception
import EDMConversions._
import com.trafigura.services.valuation.CostsAndIncomeAssignmentValuation
import com.trafigura.services.valuation.CostsAndIncomeQuotaValuation
import com.trafigura.services.valuation.PricingValuationDetails
import com.trafigura.edm.logistics.inventory.EDMInventoryItem
import com.trafigura.edm.shared.types.{Date, Quantity => EDMQuantity}


case class PhysicalMetalQuota(
  quotaID : String,
  quantity : Quantity,
  pricingSpec : TitanPricingSpec,
  // For a purchase this is the expected sale, and vice-versa - TODO find a better name
  expectedTransferPricingSpec : TitanPricingSpec) {
}


object PhysicalMetalForward{
  def apply(exchangesByGUID : Map[GUID, EDMMarket], edmMetalByGUID : Map[GUID, EDMMetal])(trade : EDMPhysicalTrade) : PhysicalMetalForward = {
    try {
      val quotas : List[PhysicalMetalQuota] = {
        trade.quotas.map(_.detail).map{
          detail =>
            val deliveryQuantity = detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
            val commodityGUIDs : Set[GUID] = detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet
            assert(commodityGUIDs.size == 1, "Trade " + trade.tradeId + " has multiple commodities")
            val deliveryDay = Day.fromLocalDate(detail.deliverySpecs.head.schedule.asInstanceOf[Date].datex)

            val pricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), exchangesByGUID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, detail.pricingSpec)
            PhysicalMetalQuota(
              detail.identifier,
              deliveryQuantity,
              pricingSpec,
              pricingSpec.dummyTransferPricingSpec
            )
        }
      }

      val isPurchase = trade.direction match {
        case EDMTrade.PURCHASE => true
        case EDMTrade.SALE => false
        case _ => throw new Exception("Trade " + trade.tradeId + " has no direction " + trade.direction)
      }
      PhysicalMetalForward(trade.tradeId, quotas, isPurchase)
    } catch {
      case ex => throw new Exception("Trade " + trade.tradeId + " failed to construct from EDM. " + ex.getMessage, ex)
    }
  }

  def value(exchangesByGUID : Map[GUID, EDMMarket], edmMetalByGUID : Map[GUID, EDMMetal], env : Environment, snapshotID : String)(trade : EDMPhysicalTrade) : Either[String, List[CostsAndIncomeQuotaValuation]] =  {

    try {
      val forward = PhysicalMetalForward(exchangesByGUID, edmMetalByGUID)(trade)
      Right(forward.costsAndIncomeValueBreakdown(env, snapshotID))
    } catch {
      case ex => Left("Error valuing trade " + trade.tradeId + ", message was " + ex.getMessage)
    }
  }
}


case class PhysicalMetalForward(tradeID : Int, quotas : List[PhysicalMetalQuota], isPurchase : Boolean) {

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
  def apply(exchangesByGUID : Map[GUID, EDMMarket],
            futuresMetalMarketByGUID : Map[GUID, EDMMetal],
            assignmentIdToQuotaMap : Map[Int, EDMQuota])
           (inventory : EDMInventoryItem) : PhysicalMetalAssignmentForward = {
    try {
      val purchaseQuota = assignmentIdToQuotaMap(inventory.purchaseAssignmentId)
      val edmPurchasePricingSpec = purchaseQuota.detail.pricingSpec
      val edmSalePricingSpec = inventory.salesAssignmentId match {
        case Some(sId) => assignmentIdToQuotaMap(sId).detail.pricingSpec
        case None => edmPurchasePricingSpec
      }

      val deliveryQuantity = purchaseQuota.detail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum
      val commodityGUIDs : Set[GUID] = purchaseQuota.detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet
      assert(commodityGUIDs.size == 1, "quota " + purchaseQuota.detail.identifier + " has multiple commodities")
      val deliveryDay = Day.fromLocalDate(purchaseQuota.detail.deliverySpecs.head.schedule.asInstanceOf[Date].datex)

      val purchasePricingSpec = EDMPricingSpecConverter(futuresMetalMarketByGUID(commodityGUIDs.head), exchangesByGUID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmPurchasePricingSpec)
      val salePricingSpec = EDMPricingSpecConverter(futuresMetalMarketByGUID(commodityGUIDs.head), exchangesByGUID).fromEdmPricingSpec(deliveryDay, deliveryQuantity, edmSalePricingSpec)
            
     val quota = PhysicalMetalQuota(
       purchaseQuota.detail.identifier,
       deliveryQuantity,
       purchasePricingSpec,
       salePricingSpec)

      PhysicalMetalAssignmentForward(inventory.oid.contents.toString, inventory, quota)
    } catch {
      case ex => throw new Exception("Inventory " + inventory.oid + " failed to construct from EDM. " + ex.getMessage, ex)
    }
  }

  def value(exchangesByGUID : Map[GUID, EDMMarket],
            futuresMetalMarketByGUID : Map[GUID, EDMMetal],
            assignmentIdToQuotaMap : Map[Int, EDMQuota],
            env : Environment, snapshotID : String)(inventory : EDMInventoryItem) : Either[String, List[CostsAndIncomeAssignmentValuation]] =  {

    try {
      val forward = PhysicalMetalAssignmentForward(exchangesByGUID, futuresMetalMarketByGUID, assignmentIdToQuotaMap)(inventory)
      Right(forward.costsAndIncomeAssignmentValueBreakdown(env, snapshotID))
    } catch {
      case ex => Left("Error valuing inventory  " + inventory.oid + ", message was " + ex.getMessage)
    }
  }
}

case class PhysicalMetalAssignmentForward(id : String, inventoryItem : EDMInventoryItem, quota : PhysicalMetalQuota) {

  def costsAndIncomeAssignmentValueBreakdown(env : Environment, snapshotID : String) : List[CostsAndIncomeAssignmentValuation] = {
  
    List(CostsAndIncomeAssignmentValuation(
      purchaseAssignmentID = inventoryItem.purchaseAssignmentId.toString,
      saleAssignmentID = inventoryItem.salesAssignmentId match { case Some(id) => Some(id.toString); case _ => None },
      snapshotID,
      quota.quantity,

      // These come from the pricing spec of the quota associated with the purchaseAssignmentID
      purchaseValuationDetails = CostsAndIncomeValuation.build(env, quota.quantity, quota.pricingSpec),
      // If saleAssignmentID is defined, then these details come from the pricing spec of its associated quota,
      // otherwise use the expected pricing spec of the purchase quota
      saleValuationDetails = CostsAndIncomeValuation.build(env, quota.quantity, quota.expectedTransferPricingSpec),

      benchmarkPremium = Quantity.NULL,
      freightParity = Quantity.NULL))
  }
}

