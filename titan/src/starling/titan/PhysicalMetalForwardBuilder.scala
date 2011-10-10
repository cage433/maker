package starling.titan

import starling.utils.Log
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.physicaltradespecs.{DeliverySpec, QuotaDetails}
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.logistics.inventory.{EDMAssignment, EDMLogisticsQuota, EDMInventoryItem}
import com.trafigura.edm.shared.types.{DateSpec, PercentageTolerance, TitanId}
import starling.titan.EDMConversions._
import com.trafigura.tradinghub.support.GUID
import org.joda.time.LocalDate
import starling.daterange.Day
import starling.instrument.physical._
import starling.quantity.{Quantity, Percentage}
import starling.db.TitanTradeSystem
import starling.instrument.{ErrorInstrument, TradeID, Trade}

class PhysicalMetalForwardBuilder(refData: TitanTacticalRefData,
            inventoryByQuotaID: Map[TitanId, List[EDMInventoryItem]],
            logisticsQuotaByQuotaID: Map[TitanId, EDMLogisticsQuota]) extends Log {
  import refData._

  private val QUOTA_PREFIX = "Q-"
  private val ASSIGNMENT_PREFIX = "A-"
  private val TRADE_PREFIX = "T-"

  private def getTitanTradeId(t: EDMPhysicalTrade): String = NeptuneId(t.titanId).identifier

  private def isPurchase(direction: String) = direction match {
    case EDMTrade.PURCHASE => true
    case EDMTrade.SALE => false
    case _ => throw new Exception(" Invalid direction " + direction)
  }

  def apply(trade: EDMPhysicalTrade, eventID : String) : List[Trade] = {
    val groupCompany = groupCompaniesByGUID.get(trade.groupCompany).map(_.name).getOrElse("Unknown Group Company")
    val counterparty = counterpartiesByGUID(trade.counterparty.counterparty).name
    val comments = trade.comments
    val contractFinalised = trade.contractFinalised.toString

    try {
      val isPurchaseTrade: Boolean = isPurchase(trade.direction)

      def deliveryQuantity(detail: QuotaDetails) = {
        detail.deliverySpecs.map {
          ds => fromTitanQuantity(ds.quantity)
        }.sum
      }
      def shapeAndGrade(spec : DeliverySpec) = {
        spec.materialSpec match {
          case rms: com.trafigura.edm.materialspecification.RefinedMetalSpec => (
            shapesByGUID(rms.shape),
            gradeByGUID(rms.grade)
            )
          case _ => throw new Exception("Expected RefinedMetalSpec, recieved " + spec.materialSpec)
        }
      }
      def tolerance(spec : DeliverySpec) = {
        val tolerance = spec.tolerance match {
          case tol: PercentageTolerance => tol
          case _ => throw new Exception("Unsupported tolerance")
        }
        def getTolerancePercentage(percentage: Option[Double]): Percentage = percentage match {
          case Some(percentage) => Percentage(percentage * 0.01) // scale since 1% in titan is 1.00 which is 100% in Starling
          case _ => Percentage(0.0)
        }
        (getTolerancePercentage(tolerance.plus.amount), getTolerancePercentage(tolerance.minus.amount))
      }
      def locations(spec : DeliverySpec) = {
        (locationsByGUID(spec.deliveryLocations.head.location), destLocationsByGUID(spec.destinationLocation))
      }
      def deliverySpec_(detail : QuotaDetails) = {
        assert(detail.deliverySpecs.size == 1, "Requires there to be a single delivery spec")
        detail.deliverySpecs.head
      }
      def deliveryDays(detail : QuotaDetails) = {
        def getDate(ds : DateSpec) : LocalDate = ds match {
          case date : com.trafigura.edm.shared.types.Date => date.value
          case _ => throw new Exception("Unsupported DateSpec type")
        }
        val contractDeliveryDay = Day.fromLocalDate(getDate(deliverySpec_(detail).schedule))
        val benchmarkDeliveryDay = Day.fromLocalDate(getDate(detail.expectedSales))
        (contractDeliveryDay, benchmarkDeliveryDay)
      }

      val submittedDay = Day.fromLocalDate(trade.submitted.toLocalDate)

      val trades: List[Trade] = {
        trade.quotas.map(_.detail).flatMap {
          detail =>
            val commodityGUIDs: Set[GUID] = detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet

            assert(commodityGUIDs.size == 1, "Trade " + getTitanTradeId(trade) + " has multiple commodities")

            val (contractDeliveryDay, benchmarkDeliveryDay) = deliveryDays(detail)
            val contractPricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), futuresExchangeByID).fromEdmPricingSpec(contractDeliveryDay, deliveryQuantity(detail), detail.pricingSpec)

            val inventoryItems = inventoryByQuotaID.get(NeptuneId(detail.identifier).titanId).flatten.toList.map(i => Inventory(i))

            val commodityName = edmMetalByGUID(commodityGUIDs.head).name
            val deliverySpec = deliverySpec_(detail)
            val (shape, grade) = shapeAndGrade(deliverySpec)

            val (tolerancePlus, toleranceMinus) = tolerance(deliverySpec)

            val (contractDeliveryLocation, benchmarkDeliveryLocation) = locations(deliverySpec)
            val quotaQuantity : Quantity = deliverySpec.quantity
            val quotaID = NeptuneId(detail.identifier).identifier
            def makeAssignment(ass: EDMAssignment, inv: Inventory, isPurchase: Boolean) = {
              PhysicalMetalAssignment(
                ass.oid.contents.toString,
                inv.assignmentQuantity,
                commodityName,
                contractDeliveryDay,
                contractPricingSpec,
                contractDeliveryLocation.name,
                if (inv.isAllocated) None else Some(benchmarkDeliveryDay),
                if (inv.isAllocated) None else Some(benchmarkDeliveryLocation.name),
                isPurchase,
                inventoryID = ass.inventoryId.toString,
                inventoryQuantity = inv.currentQuantity,
                grade = grade.name
              )
            }
            def makeTradeAttributes(inventoryID : Option[String]) = {
              TitanTradeAttributes(
                  quotaID,
                  quotaQuantity,
                  getTitanTradeId(trade),
                  inventoryID,
                  groupCompany,
                  comments,
                  submittedDay,
                  shape.name,
                  contractFinalised,
                  tolerancePlus,
                  toleranceMinus,
                  eventID
              )
            }
            if (isPurchaseTrade) {
              val trades = inventoryItems.map {
                inv =>
                  val assignment = makeAssignment(inv.item.purchaseAssignment, inv, true)
                  val attributes = makeTradeAttributes(Some(inv.id))
                  Trade(
                    TradeID(ASSIGNMENT_PREFIX + inv.item.purchaseAssignment.oid.contents.toString, TitanTradeSystem),
                    submittedDay,
                    counterparty,
                    attributes,
                    assignment
                  )
              }
              trades
            }
            else {
              val logisticsQuota: Option[EDMLogisticsQuota] = logisticsQuotaByQuotaID.get(NeptuneId(detail.identifier).titanId)
              log.info("detail.identifier %s logistics quota was %s ".format(NeptuneId(detail.identifier).identifier, logisticsQuota))
              val isFullyAllocated = logisticsQuota.map(_.fullyAllocated).getOrElse(false)
              val assignmentTrades = inventoryItems.map {
                inv =>
                  val assignment = makeAssignment(inv.item.salesAssignment, inv, false)
                  val attributes = makeTradeAttributes(Some(inv.id))
                  Trade(
                    TradeID(ASSIGNMENT_PREFIX + inv.item.salesAssignment.oid.contents.toString, TitanTradeSystem),
                    submittedDay,
                    counterparty,
                    attributes,
                    assignment
                  )
              }
              val unassignedSalesQuota = if (isFullyAllocated)
                None
              else {
                val unallocatedQuantity = deliveryQuantity(detail) - assignmentTrades.map(_.tradeable.asInstanceOf[PhysicalMetalAssignment].quantity).sum

                val unallocatedQuota = UnallocatedSalesQuota(
                  unallocatedQuantity,
                  commodityName,
                  contractDeliveryDay,
                  contractPricingSpec,
                  contractDeliveryLocation.name,
                  Some(benchmarkDeliveryDay),
                  Some(benchmarkDeliveryLocation.name),
                  grade.name
                )
                val attributes = makeTradeAttributes(None)
                Some(Trade(
                  TradeID(QUOTA_PREFIX + quotaID, TitanTradeSystem),
                  submittedDay,
                  counterparty,
                  attributes,
                  unallocatedQuota
                ))
              }
              unassignedSalesQuota.toList ::: assignmentTrades
            }
        }
      }

      trades
    }
    catch {
      case e => {
            List(Trade(TradeID(TRADE_PREFIX + trade.titanId.value, TitanTradeSystem),
                TitanTradeAttributes.dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(trade, eventID),
                new ErrorInstrument(e.getMessage)))
        }
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

