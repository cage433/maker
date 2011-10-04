package starling.titan.valuation

import starling.quantity.Quantity
import starling.daterange.Day




case class PricingValuationDetails(
  // currently excludes premium - need to check with business
  price : Quantity,
  premium : Quantity,
  // currently excludes premium - need to check with business
  value: Quantity,
  isComplete : Boolean,
  fixedQuantity : Quantity,
  pricingType : String,
  quotationPeriodStart : Option[Day],
  quotationPeriodEnd : Option[Day],
  index : String
)

trait CostsAndIncomeValuation {
  def snapshotID : String
  def purchaseValuationDetails : PricingValuationDetails
  def saleValuationDetails : PricingValuationDetails
  def benchmarkPremium : Quantity
  def freightParity : Quantity
}

trait AssignmentValuation{
  def assignmentID : String
}

case class CostsAndIncomeAllocatedPurchaseAssignmentValuation
(
  assignmentID : String,
  /*
     Rows 71, 72, 7/home/alex/workspace/dev/services/starling/titan/src/starling/titan/valuation4
     For a purchase assignment this is the final receipted quantity (when it exists) otherwise current inventory quantity
     For a salee assignment this is the final delivered quantity (when it exists) otherwise current inventory quantity
   */
  assignmentQuantity : Quantity,
  valuationDetails : Either[String, PricingValuationDetails]
) extends AssignmentValuation

case class CostsAndIncomeAllocatedSaleAssignmentValuation
(
  assignmentID : String,
  /*
     Rows 71, 72, 74
     For a purchase assignment this is the final receipted quantity (when it exists) otherwise current inventory quantity
     For a salee assignment this is the final delivered quantity (when it exists) otherwise current inventory quantity
   */
  assignmentQuantity : Quantity,
  valuationDetails : Either[String, PricingValuationDetails],
  weightGainOrLoss : Quantity,
  weightGainOrLossDetails : Either[String, PricingValuationDetails]
) extends AssignmentValuation

/**
* A valuation for a particular inventory assigments and snapshot
*/
case class CostsAndIncomeInventoryValuation(
  snapshotID : String,
  // These come from the pricing spec of the quota associated with the purchaseAssignmentID
  purchaseValuationDetails : PricingValuationDetails,
  // If saleAssignmentID is defined, then these details come from the pricing spec of its associated quota,
  // otherwise use the expected pricing spec of the purchase quota
  saleValuationDetails : PricingValuationDetails,
  benchmarkPremium : Quantity,
  freightParity : Quantity,
  purchaseAssignmentID : String,
  saleAssignmentID : Option[String],
  purchaseQuantity : Quantity,
  saleQuantity : Quantity) extends CostsAndIncomeValuation

case class CostsAndIncomeUnallocatedAssignmentValuation
(
  assignmentID : String,
  /*
    Row 71
    Defined using the same rule as CostsAndIncomeAllocatedAssignmentValuation, however note that unallocated
    assignments only exist on the purchase side.
   */
  assignmentQuantity : Quantity,
  /*
    Rows 73, 75
    current inventory quantity - assignmentQuantity
   */
  valuationDetails : Either[String, PricingValuationDetails],
  benchmarkDetails : Either[String, PricingValuationDetails],
  weightGainOrLoss : Quantity,
  weightGainOrLossDetails : Either[String, PricingValuationDetails],
  freightParity : Quantity
) extends AssignmentValuation

trait QuotaValuation {
  def quotaID: String
}

case class PurchaseQuotaValuation(
 quotaID: String,
  /*
    Sum of unallocated AND allocated assignment quantities - Row 64
   */
 quantity: Quantity,
  /*
    Sum of unallocated assignment quantities  - Row 65
   */
 unallocatedQuantity : Quantity,
  /*
    Sum of unallocated weight gain/loss quantities  - Row 66
   */
 weightGainOrLoss : Quantity,
 unallocatedValuations: Map[String, CostsAndIncomeUnallocatedAssignmentValuation], // Keys are assignment ID
 assignmentValuations: Map[String, CostsAndIncomeAllocatedPurchaseAssignmentValuation] // Keys are assignment ID
) extends QuotaValuation


case class SalesQuotaValuation(
  quotaID : String,
  isFullyAllocated : Boolean,
  /**
   * Row 67 and Row 70
   * Some(Contactual quantity - allocated quantity when not fully allocated), otherwise None
   */
  unallocatedQuantity : Option[Quantity],

  /**
   * Left indicates an error,
   * Right[None] when the quota is fully allocated
   * Right[Some] when the quota is not fully allocated. 
   */
  unallocatedValuationDetails : Either[String, Option[PricingValuationDetails]],
  benchmarkDetails : Either[String, Option[PricingValuationDetails]],

  /**
   * Row 68
   * Sum of allocated assignment quantities
   */
  allocatedQuantity : Quantity,

  /**
   * Map of assignment ID to its valuation. This is necessarily allocated.
   */
  assignmentValuations : Map [String, CostsAndIncomeAllocatedSaleAssignmentValuation], 

  /*
    Row 69
    For unallocated assignments, sum of weight gain/loss
   */
  weightGainOrLoss : Quantity
) extends QuotaValuation


