package starling.titan

import com.trafigura.tradinghub.support.GUID
import com.trafigura.edm.tradeservice.EdmGetTrades
import com.trafigura.edm.materialspecification.CommoditySpec
import starling.systemofrecord.SystemOfRecord
import starling.daterange.Day
import starling.pivot.Field
import starling.instrument.{Trade, TradeID, TradeAttributes}
import starling.db.TitanTradeSystem
import java.lang.UnsupportedOperationException
import starling.instrument.{ErrorInstrument, Costs, Tradeable}
import com.trafigura.edm.logistics.inventory._
import com.trafigura.edm.shared.types.{TitanId, Date, DateSpec, PercentageTolerance}
import starling.quantity.Percentage
import org.joda.time.LocalDate
import com.trafigura.edm.shared.types.Quantity
import com.trafigura.tradecapture.internal.refinedmetal._
import starling.utils.conversions.RichMapWithErrors._
import StarlingTradeAssignment._
import starling.utils.{Stopwatch, Log, StackTraceToString}
import com.trafigura.services.valuation.TradeManagementCacheNotReady
import com.trafigura.edm.trades.{CompletedTradeState, PhysicalTrade => EDMPhysicalTrade, Trade => EDMTrade}
import com.trafigura.edm.physicaltradespecs.{QuotaDetails, DeliverySpec, PhysicalTradeQuota}
import starling.instrument.physical._
import starling.titan.EDMConversions._

class ExternalTitanServiceFailed(cause : Throwable) extends Exception(cause)

case class StarlingTradeAssignment(quotaName : String, quantity : Quantity, assignmentID : Option[String] = None, inventoryID : Option[String] = None) {
  def id = starlingTradeId(assignmentID, quotaName)
  private def starlingTradeId(assignmentID : Option[String], quotaName : String) : String = {
    assignmentID match {
      case Some(id) => assignmentPrefix + id
      case None => quotaPrefix + quotaName
    }
  }
}
case object StarlingTradeAssignment {
  val assignmentPrefix = "A-"
  val quotaPrefix = "Q-"
}

// for some strange reason EDM trade service converts titan quota ID with prefix NEPTUNE:
case class NeptuneId(id : String) {
  import NeptuneId._
  def identifier : String = identifier(id)
  def titanId : TitanId = TitanId(identifier)
  def identifier(ident : String) : String = ident match {
    case i : String if i != null => {
      ident.substring(neptunePrefix.length)
    }
    case null => null
  }
}
object NeptuneId {
  val neptunePrefix = "NEPTUNE:"
  def apply(titanId : TitanId) : NeptuneId = {
    NeptuneId(titanId.value)
  }
}

case class Inventory(item: EDMInventoryItem) {

  val receivedQuantity: Option[Quantity] = {
    item.status match {
      case ExpectedEDMInventoryItemStatus => None
      case SplitEDMInventoryItemStatus | CancelledEDMInventoryItemStatus => throw new Exception("Unexpected inventory status " + item.status)
      case _ => Some(item.purchaseAssignment.quantity)
    }
  }

  def assignmentQuantity: Quantity = {
    receivedQuantity.getOrElse(item.quantity)
  }

  def isAllocated = Option(item.salesAssignment).isDefined

  def currentQuantity = item.quantity
}

object PhysicalMetalForwardBuilder extends Log {

  private def getTradeId(t: EDMPhysicalTrade): String = Option(t.titanId).map(_.value).getOrElse("<null>")


  def isPurchase(direction: String): Boolean = {
    val isPurchase = direction match {
      case EDMTrade.PURCHASE => true
      case EDMTrade.SALE => false
      case _ => throw new Exception(" Invalid direction " + direction)
    }
    isPurchase
  }

  def apply(refData: TitanTacticalRefData,
            inventoryByQuotaID: Map[TitanId, List[EDMInventoryItem]] = Map(),
            logisticsQuotaByQuotaID: Map[TitanId, EDMLogisticsQuota] = Map())
           (trade: EDMPhysicalTrade): PhysicalMetalForward = {

    import refData._
    val groupCompany = groupCompaniesByGUID.get(trade.groupCompany).map(_.name).getOrElse("Unknown Group Company")
    val counterparty = counterpartiesByGUID(trade.counterparty.counterparty).name

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
          case Some(percentage) => Percentage(percentage)
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


      val quotas: List[PhysicalMetalQuota] = {
        trade.quotas.map(_.detail).map {
          detail =>
            val commodityGUIDs: Set[GUID] = detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet

            assert(commodityGUIDs.size == 1, "Trade " + getTradeId(trade) + " has multiple commodities")

            val (contractDeliveryDay, benchmarkDeliveryDay) = deliveryDays(detail)
            val contractPricingSpec = EDMPricingSpecConverter(edmMetalByGUID(commodityGUIDs.head), futuresExchangeByID).fromEdmPricingSpec(contractDeliveryDay, deliveryQuantity(detail), detail.pricingSpec)

            val inventoryItems = inventoryByQuotaID.get(detail.identifier).flatten.toList.map(i => Inventory(i))

            val commodityName = edmMetalByGUID(commodityGUIDs.head).name
            val deliverySpec = deliverySpec_(detail)
            val (shape, grade) = shapeAndGrade(deliverySpec)

            val (tolerancePlus, toleranceMinus) = tolerance(deliverySpec)

            val (deliveryLocation, destinationLocation) = locations(deliverySpec)

            def makeQuota(quotaID: String, assignments: List[PhysicalMetalAssignment],
                          unallocatedSale: Option[UnallocatedSalesQuota], isPurchase: Boolean) = {
              PhysicalMetalQuota(
                isPurchase, quotaID, assignments, unallocatedSale,
                deliveryLocation.name,
                destinationLocation.name,
                shape.name,
                grade.name,
                tolerancePlus,
                toleranceMinus,
                contractDeliveryDay,
                benchmarkDeliveryDay
              )
            }
            def makeAssignment(ass: EDMAssignment, inv: Inventory, isPurchase: Boolean,
                               benchmarkPricingSpec: Option[TitanPricingSpec]) = {
              PhysicalMetalAssignment(
                ass.oid.contents.toString,
                commodityName,
                inv.assignmentQuantity,
                contractDeliveryDay,
                contractPricingSpec,
                if (inv.isAllocated) None else Some(benchmarkDeliveryDay),
                benchmarkPricingSpec,
                isPurchase,
                inventoryID = ass.inventoryId.toString,
                inventoryQuantity = inv.currentQuantity
              )
            }
            if (isPurchaseTrade) {
              val assignments = inventoryItems.map {
                inv =>
                  val benchmarkPricingSpec = Option(inv.item.salesAssignment).map {
                    _ => contractPricingSpec.dummyTransferPricingSpec
                  }
                  makeAssignment(inv.item.purchaseAssignment, inv, true, benchmarkPricingSpec)
              }

              makeQuota(
                detail.identifier.value,
                assignments,
                None,
                isPurchase = true
              )
            } else {
              val logisticsQuota: Option[EDMLogisticsQuota] = logisticsQuotaByQuotaID.get(detail.identifier)
              val isFullyAllocated = logisticsQuota.map(_.fullyAllocated).getOrElse(false)
              val assignments = inventoryItems.map {
                inv => makeAssignment(inv.item.salesAssignment, inv, false, None)
              }
              val unassignedSalesQuota = if (isFullyAllocated)
                None
              else {
                val unallocatedQuantity = deliveryQuantity(detail) - assignments.map(_.quantity).sum

                Some(UnallocatedSalesQuota(
                  commodityName,
                  unallocatedQuantity,
                  contractDeliveryDay,
                  contractPricingSpec,
                  Some(benchmarkDeliveryDay),
                  Some(contractPricingSpec.dummyTransferPricingSpec)
                ))
              }
              makeQuota(
                detail.identifier.value,
                assignments,
                unassignedSalesQuota,
                isPurchase = false
              )
            }
        }
      }


      PhysicalMetalForward(getTradeId(trade), groupCompany, counterparty, quotas, isPurchaseTrade)
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
  //  def valueWithAssignments(exchangesByID : Map[String, EDMMarket], edmMetalByGUID : Map[GUID, EDMMetal], env : Environment, snapshotID : String)(trade : EDMPhysicalTrade) : Either[String, List[QuotaValuation]] = {
  //    try {
  //      val forward = PhysicalMetalForward(exchangesByID, edmMetalByGUID)(trade)
  //      Right(forward.costsAndIncomeQuotaValueBreakdown(env, snapshotID))
  //    }
  //    catch {
  //      case ex => {
  //        Log.warn("Error valuing trade " + getTradeId(trade) + ", message was " + ex.getMessage)
  //        Left("Error valuing trade " + getTradeId(trade) + ", message was " + ex.getMessage)
  //      }
  //    }
  //  }

}

class TitanSystemOfRecord(
  titanTradeServices : TitanServices,
  logisticsServices : TitanLogisticsServices
)
  extends SystemOfRecord with Log
{

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {

    //val assignments = logisticsServices.assignmentService.service.getAllAssignments()
    val edmTrades : List[EDMPhysicalTrade] = titanTradeServices.getAllCompletedTrades()
    println("No of edm tardes = " + edmTrades.size)
    val inventory = logisticsServices.inventoryService.service.getAllInventoryLeaves()

    def quotaNames(inventory : EDMInventoryItem) : List[String] = inventory.purchaseAssignment.quotaName :: Option(inventory.salesAssignment).map(_.quotaName).toList

    val quotaNamesForInventory : List[(List[TitanId], EDMInventoryItem)] = inventory.map(i => (quotaNames(i), i)).map(i => i._1.map(qn => TitanId(qn)) -> i._2)
    val quotaToInventory : List[(TitanId, EDMInventoryItem)] = quotaNamesForInventory.flatMap(i => i._1.map(x => (x -> i._2)))
    val inventoryByQuotaID : Map[TitanId, List[EDMInventoryItem]] = quotaToInventory.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
    val logisticsQuotaByQuotaID : Map[TitanId, EDMLogisticsQuota] = Map() // this isn't currently implemented, probably best to complete after refactoring is completed

    val tradeForwardBuilder = PhysicalMetalForwardBuilder(titanTradeServices, inventoryByQuotaID, logisticsQuotaByQuotaID) _
    val allTrades : List[Trade] = edmTrades.flatMap{
      edmTrade =>
        try {
          val fwd : PhysicalMetalForward = tradeForwardBuilder(edmTrade)
          fwd.asStarlingTrades
        } catch {
          case e =>
            try {
              println("Got an exception")
              List(Trade(
                    TradeID(edmTrade.titanId.value, TitanTradeSystem),
                    TitanTradeAttributes.dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(edmTrade), new ErrorInstrument(e.getMessage)))
            } catch {
              case e =>
                println("Weird exception")
                throw e
            }
        }
    }


    allTrades.map(f)

    val tradeErrors = allTrades.map(_.tradeable).collect{case ErrorInstrument(err) => err}
    (tradeErrors.size, tradeErrors.toSet)
  }

  def trade(id: String)(f: (Trade) => Unit) {
//    import StarlingTradeAssignment._
//    val tradeAssignment = if (id.startsWith(assignmentPrefix)) {
//      val edmAssignment = logisticsServices.assignmentService.service.getAssignmentById(id.substring(assignmentPrefix.size).toInt)
//      val quota = quotaMap(edmAssignment.quotaName)
//      tc.toTrade(id, quota.detail.identifier.value, edmAssignment.quantity, Some(edmAssignment.oid.toString), Some(edmAssignment.inventoryId.toString))
//    }
//    else if (id.startsWith(quotaPrefix)) {
//      val quotaName = id.substring(quotaPrefix.size)
//      val quota = quotaMap(quotaName)
//      tc.toTrade(id, quota.detail.identifier.value, quota.detail.pricingSpec.quantity, None, None)
//    }
//    else {
//      throw new Exception("Unexpeced trade id " + id)
//    }

//    f(tradeAssignment)
  }

  protected def readers = throw new UnsupportedOperationException()
}


object TradeConverter{
  def apply(refData : TitanTacticalRefData, titanTradeCache : TitanTradeCache) : TradeConverter = {
    new TradeConverter(refData, titanTradeCache.quotaNameToTradeMap, titanTradeCache.quotaNameToQuotaMap)
  }
}

/**
 * EDM Inventory assignment to Starling "Trade" conversions
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
    val deliveryLocation = refData.locationsByGUID(deliverySpec.deliveryLocations.head.location)
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

    val schedule = Day.fromLocalDate(getDate(deliverySpec.schedule))
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
      deliveryLocation.name,
      destinationLocation.name,
      contractFinalised,
      tolerancePlus,
      toleranceMinus,
      schedule,
      expectedSales
    )

    import EDMConversions._

    val metal : Metal = refData.edmMetalByGUID(quotaDetail.deliverySpecs.head.materialSpec.asInstanceOf[CommoditySpec].commodity)
    val priceSpecConverter = EDMPricingSpecConverter(metal, refData.futuresExchangeByID)

    // todo - this is the wrong quantity at the assignment level??
    val deliveryQuantity = quantity // = quotaDetail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum

    val deliveryDay = Day.fromLocalDate(quotaDetail.deliverySpecs.head.schedule.asInstanceOf[Date].value)

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
      inventoryQuantity = null
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

case class TitanTradeAttributes(
  assignmentID : Option[String],
  quotaID : String,
  titanTradeID : String,
  inventoryID : Option[String],
  groupCompany : String,
  comment : String,
  submitted : Day,
  shape : String,
  grade : String,
  deliveryLocation : String,
  destinationLocation : String,
  contractFinalised : String,
  tolerancePlus : Percentage,
  toleranceMinus : Percentage,
  schedule : Day,
  expectedSales : Day
) 
    extends TradeAttributes 
{

  if (quotaID == null) {
    println("quota was null")
  }
  require(quotaID != null, "quotaID cannot be null")
  require(titanTradeID != null, "titanTradeID cannot be null")

  import TitanTradeStore._
  
  def details = Map(
    quotaID_str -> quotaID,
    titanTradeID_str -> titanTradeID,
    groupCompany_str -> groupCompany,
    comment_str -> comment,
    submitted_str -> submitted,
    shape_str -> shape,
    grade_str -> grade,
    deliveryLocation_str -> deliveryLocation,
    destinationLocation_str -> destinationLocation,
    contractFinalised_str -> contractFinalised,
    tolerancePlus_str -> tolerancePlus,
    toleranceMinus_str -> toleranceMinus,
    schedule_str -> schedule,
    expectedSales_str -> expectedSales
  ) ++ assignmentID.map{id => assignmentID_str -> id} ++ inventoryID.map{id => inventoryID_str -> id}

  override def createFieldValues = details.map{
    case (k, v) => Field(k) -> v
  }
  
}

object TitanTradeAttributes{
  val dummyDate = Day(1980, 1, 1)
  def errorAttributes(edmTrade : EDMPhysicalTrade) = {
    val titanTradeID = edmTrade.titanId.value

    TitanTradeAttributes(
      assignmentID = None,
      quotaID = "",
      titanTradeID = titanTradeID,
      inventoryID = None,
      groupCompany = "",
      comment = "",
      submitted = dummyDate,
      shape = "",
      grade = "",
      deliveryLocation = "",
      destinationLocation = "",
      contractFinalised = "",
      tolerancePlus = Percentage(0),
      toleranceMinus = Percentage(0),
      schedule = dummyDate,
      expectedSales = dummyDate
    )
  }
}


/**
 * logistics service interface
 */
object LogisticsServices {
  type EdmAssignmentServiceWithGetAllAssignments = EdmAssignmentService with Object { def getAllAssignments() : List[EDMAssignment] }
  type EdmInventoryServiceWithGetAllInventory = EdmInventoryService with Object {
    def getAllInventoryLeaves() : List[EDMInventoryItem]
    def getAllInventory() : LogisticsInventoryResponse
  }
}

import LogisticsServices._

trait TitanLogisticsAssignmentServices extends ServiceProxy[EdmAssignmentServiceWithGetAllAssignments]
trait TitanLogisticsInventoryServices extends ServiceProxy[EdmInventoryServiceWithGetAllInventory]

trait ServiceProxy[T] {
  val service : T
}

trait TitanLogisticsServices {
  val assignmentService : TitanLogisticsAssignmentServices
  val inventoryService : TitanLogisticsInventoryServices
}

/**
 * Trade cache provide trade map lookup by trade id and also a quota id to trade map lookup
 */
trait TitanTradeCache {
  protected var tradeMap: Map[TitanId, EDMPhysicalTrade]
  protected var quotaIDToTradeIDMap: Map[String, TitanId]
  def getTrade(id: TitanId): EDMPhysicalTrade
  def getAllTrades(): List[EDMPhysicalTrade]
  def removeTrade(id : TitanId) {
    tradeMap = tradeMap - id
    quotaIDToTradeIDMap = quotaIDToTradeIDMap.filter{ case (_, value) => value != id}
  }

  def addTrade(id : TitanId) {
    tradeMap += id -> getTrade(id)
    addTradeQuotas(id)
  }

  def addTradeQuotas(id : TitanId) {
    val trade = tradeMap(id)
    quotaIDToTradeIDMap ++= trade.quotas.map{quota => (quota.detail.identifier.value, id)}
  }

  def tradeIDFromQuotaID(quotaID: String): TitanId

  def quotaNameToTradeMap : Map[String, EDMPhysicalTrade] =
    getAllTrades().flatMap{trade =>  trade.asInstanceOf[EDMPhysicalTrade].quotas.map{q => NeptuneId(q.detail.identifier.value).identifier -> trade}}.toMap.withException()
  def quotaNameToQuotaMap : Map[String, PhysicalTradeQuota] =
    getAllTrades().flatMap{trade =>  trade.asInstanceOf[EDMPhysicalTrade].quotas.map{q => NeptuneId(q.detail.identifier.value).identifier -> q}}.toMap.withException()
}

/**
 * Tactical ref data, service proxies / data
 *   also includes the trademgmt EDM trade serivce, this should be refactored to  separate out at some point
 */
trait TitanTacticalRefData {

  val edmMetalByGUID: Map[GUID, Metal]
  val futuresExchangeByID: Map[String, Market]
  val counterpartiesByGUID: Map[GUID, Counterparty]
  val shapesByGUID : Map[GUID, Shape]
  val gradeByGUID : Map[GUID, Grade]
  val locationsByGUID : Map[GUID, Location]
  val destLocationsByGUID : Map[GUID, DestinationLocation]
  val groupCompaniesByGUID : Map[GUID, GroupCompany]
}

trait TitanEdmTradeService extends Log{
  val titanGetEdmTradesService : EdmGetTrades

 def getTrade(id : TitanId) : EDMPhysicalTrade = {
    try {
      titanGetEdmTradesService.get(id).asInstanceOf[EDMPhysicalTrade]
    }
    catch {
      case e : Throwable => throw new ExternalTitanServiceFailed(e)
    }
  }

  def getAllCompletedTrades() : List[EDMPhysicalTrade] = {
    try {
      val sw = new Stopwatch()
      val edmTradeResult = titanGetEdmTradesService.getAll()
      log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
      if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
      log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
      val edmTrades = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade]).flatMap(Option(_)).filter(pt => pt.state == CompletedTradeState)

      // temporary code, trademgmt are sending us null titan ids
      val (nullIds, validIds) = edmTrades.span(_.titanId == null)
      if (nullIds.size > 0) {
        log.error("Null Titan trade IDs found!")
        log.error("null ids \n%s\n%s".format(nullIds, validIds))
        //assert(false, "Null titan ids found - fatal error")
      }
      edmTrades
    }
    catch {
      case e : Throwable => throw new ExternalTitanServiceFailed(e)
    }
  }
}

trait TitanServices extends TitanTacticalRefData with TitanEdmTradeService
