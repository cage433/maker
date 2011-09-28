package starling.instrument.physical

import starling.curves.Environment
import starling.instrument.Tradeable
import starling.daterange.{Day, DayAndTime}
import starling.titan.valuation.{PricingValuationDetails, SalesQuotaValuation, PurchaseQuotaValuation, QuotaValuation}
import starling.quantity.{Percentage, Quantity}


case class PhysicalMetalQuota(
  isPurchase : Boolean,
  quotaID : String,
  assignments : List[PhysicalMetalAssignment],
  unallocatedSalesPortion : Option[UnallocatedSalesQuota],
  deliveryLocation : String,
  destinationLocation : String,
  shape : String,
  grade : String,
  tolerancePlus : Percentage,
  toleranceMinus : Percentage,
  contractDeliveryDay : Day,
  benchmarkDeliveryDay : Day
) {

  def fullyAllocated = {
    assert(!isPurchase, "Full allocation applies only to sales")
    !unallocatedSalesPortion.isDefined
  }



  // the unallocated quota quantity
//  def unallocatedQuotaQuantity : Quantity = {
//    assignments.find(! _.isAllocated).map(_.assignmentQuantity).getOrElse(Quantity.NULL)
//    /**
//     * work out the quota level quantities
//     *   from the direction and rules around allocated quantities and fully allocated status
//     */
//    if (isPurchase) {
//      unallocatedAssignments.map(_.assignmentQuantity).sum
//    } else {
//        if (!fullyAllocated)
//          quantity - allocatedAssignmentQuantity
//        else
//          quantity * 0 // quota level quantity is zero when fully allocated on the sales side
//    }
//  }

  // using actual assignment level quantities
//  def allocatedAssignmentQuantity : Quantity = {
//    allocatedAssignments.map{a => fromTitanQuantity(a.assignmentQuantity)}.sum
//  }

//  private def makeAssignment(a : EDMAssignment, i : Inventory) = PhysicalMetalAssignment(a, i, contractPricingSpec, benchmarkPricingSpec)
//  private def allocatedAssignments : List[PhysicalMetalAssignment] = {
//    if (isPurchase) {
//      // return purchase assignments that have been allocated to a corresponding sales assignment via the assigned inventory
//      inventory.filter(i => Option(i.item.salesAssignment).isDefined).map{i => makeAssignment(i.item.purchaseAssignment, i)}
//    }
//    else
//      // return the sales assignments (all)
//      inventory.map{i =>
//        makeAssignment(i.item.salesAssignment, i)
//      }
//  }

//  private def unallocatedAssignments : List[PhysicalMetalAssignment] = {
//    if (isPurchase)
//      inventory.filterNot(i => Option(i.item.salesAssignment).isDefined).map{i => makeAssignment(i.item.purchaseAssignment, i)}
//    else
//      Nil
//  }
//  private implicit def toValuationMap[A <: AssignmentValuation](values : List[A]) : Map[String, A] = values.map{v => v.assignmentID -> v}.toMap

  lazy val (allocatedAssignments, unallocatedAssignments) =  assignments.partition(_.isAllocated)

  lazy val allocatedAssignmentQuantity = allocatedAssignments.map(_.quantity).sum
  def value(env : Environment) : QuotaValuation = {
    if (isPurchase) {
        val unallocatedQuotaQuantity = unallocatedAssignments.map(_.quantity).sum
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
         unallocatedValuations = unallocatedAssignments.map{ass => (ass.assignmentID -> ass.unallocatedPurchaseValue(env))}.toMap,
         assignmentValuations = allocatedAssignments.map{ ass => (ass.assignmentID -> ass.allocatedPurchaseValue(env))}.toMap
      )
    } else {
        val unallocatedQuotaQuantity = unallocatedSalesPortion.map(_.quantity)
        val (unallocatedValuationDetails, benchmarkDetails) = unallocatedSalesPortion match {
          case Some(s) => {

            def valueSpec(spec : TitanPricingSpec) : Either[String, Option[PricingValuationDetails]] = {
              CostsAndIncomeValuation.buildEither(env, unallocatedQuotaQuantity.get, spec) match {
                case Right(r) => Right(Some(r))
                case Left(errorMessage) => Left(errorMessage)
              }
            }
            (valueSpec(s.contractPricingSpec), valueSpec(s.benchmarkPricingSpec.get))
          }
          case None => (Right(None), Right(None))
        }
        SalesQuotaValuation(
          quotaID,
          fullyAllocated,
          /*
            Row 67 and Row 70
            Contactual quantity - allocated quantity when not fully allocated, otherwise 0
           */
          unallocatedQuotaQuantity,
          unallocatedValuationDetails,
          benchmarkDetails,
          /*
            Row 68
            Sum of allocated assignment quantities
           */
          allocatedQuantity = allocatedAssignmentQuantity,
          assignmentValuations = allocatedAssignments.map{ ass => (ass.assignmentID -> ass.allocatedSaleValue(env))}.toMap,

          /*
            Row 69
            For unallocated assignments, sum og weight gain/loss
           */
          weightGainOrLoss = allocatedAssignments.map(_.weightGain).sum
        )
    }
  }
}

