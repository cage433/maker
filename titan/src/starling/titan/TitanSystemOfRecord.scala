package starling.titan

import com.trafigura.tradinghub.support.GUID
import com.trafigura.edm.tradeservice.EdmGetTrades
import com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade, Trade => EDMTrade}
import starling.systemofrecord.SystemOfRecord
import starling.daterange.Day
import starling.pivot.Field
import starling.instrument.{Trade, TradeID, TradeAttributes}
import starling.instrument.physical.PhysicalMetalAssignment
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
import starling.utils.{Log, StackTraceToString}

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

class TitanSystemOfRecord(
  titanTradeCache : TitanTradeCache,
  refData : TitanTacticalRefData,
  logisticsServices : TitanLogisticsServices) extends SystemOfRecord with Log {

  lazy val quotaNameToTradeMap : Map[String, EDMPhysicalTrade] = titanTradeCache.quotaNameToTradeMap
  lazy val quotaMap = titanTradeCache.quotaNameToQuotaMap

  lazy val tc = TradeConverter(refData, titanTradeCache)

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {
    
    val assignments = logisticsServices.assignmentService.service.getAllAssignments()

    // check for a null quota id key, this should not happen and could cause errors later on
    quotaMap.get(null) match {
      case Some(q) => log.warn("Found null quota names from trade cache quota map, " + q)
      case None => 
    }
    
    val assignedQuotaNames = assignments.map(_.quotaName)
    val unassignedQuotas = quotaMap.filterKeys(k => !assignedQuotaNames.exists(_ == k))

    // don't think this is a valid state - todo review this check for validity,
    //   until we know more, it will produce a warning if we find purchase quotas with no assignments at all
    val unassignedPurchaseQuotas = unassignedQuotas.keys.map(k => quotaNameToTradeMap(k)).filter(_.direction == EDMTrade.PURCHASE)
    if (unassignedPurchaseQuotas.size > 0)
      log.warn("unexpected unassigned puchase quota found in the dataset, quotaName = " + unassignedPurchaseQuotas.mkString(", "))

    /**
     * create a list of all "assignments, some real and some dummy (unallocated) assignments representing unallocated sales assignments"
     */
    val actualAssignments : List[StarlingTradeAssignment] = assignments.map(a => StarlingTradeAssignment(a.quotaName, a.quantity, Some(a.oid.contents.toString), Some(a.inventoryId.toString)))
    val dummyAssignments : List[StarlingTradeAssignment] = unassignedQuotas.map{ case (k, v) => StarlingTradeAssignment(Option(k).getOrElse("<Null Quota ID>"), v.detail.pricingSpec.quantity)}.toList
    val allAssignments : List[StarlingTradeAssignment] = actualAssignments ::: dummyAssignments

    val tradeErrors = allAssignments.map(assignment => {
      try {
        val trade = tc.toTrade(assignment)
        f(trade)
        Right(assignment.id)
      }
      catch {
        case ex : ExternalTitanServiceFailed => throw ex.getCause
        case ex : Throwable => {
          val errorInstrument = ErrorInstrument(StackTraceToString.string(ex).trim)
          val dummyDate = Day(1980, 1, 1)

          val errorTrade = Trade(
            TradeID(assignment.id, TitanTradeSystem),
            dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(assignment.quotaName, Some(assignment), quotaNameToTradeMap.get(assignment.quotaName)), errorInstrument)
          f(errorTrade)
          Left(if (ex.getMessage == null) "Null Error Message" else ex.getMessage)
        }
      }
    }).collect({ case Left(err) => err }).distinct
    
    (tradeErrors.size, tradeErrors.toSet)
  }

  def trade(id: String)(f: (Trade) => Unit) {
    import StarlingTradeAssignment._
    val tradeAssignment = if (id.startsWith(assignmentPrefix)) {
      val edmAssignment = logisticsServices.assignmentService.service.getAssignmentById(id.substring(assignmentPrefix.size).toInt)
      val quota = quotaMap(edmAssignment.quotaName)
      tc.toTrade(id, quota.detail.identifier.value, edmAssignment.quantity, Some(edmAssignment.oid.toString), Some(edmAssignment.inventoryId.toString))
    }
    else if (id.startsWith(quotaPrefix)) {
      val quotaName = id.substring(quotaPrefix.size)
      val quota = quotaMap(quotaName)
      tc.toTrade(id, quota.detail.identifier.value, quota.detail.pricingSpec.quantity, None, None)
    }
    else {
      throw new Exception("Unexpeced trade id " + id)
    }

    f(tradeAssignment)
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
      metal.name,
      deliveryQuantity,
      deliveryDay,
      priceSpecConverter.fromEdmPricingSpec(deliveryDay, deliveryQuantity, quotaDetail.pricingSpec))

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
  def errorAttributes(quotaName : String, assignment : Option[StarlingTradeAssignment], edmTrade : Option[EDMPhysicalTrade]) = {
    val dummyDate = Day(1980, 1, 1)
    val titanTradeID = edmTrade match {
      case Some(trade) => trade.titanId.value
      case None => "Unknown"
    }

    TitanTradeAttributes(
      assignmentID = assignment.flatMap(_.assignmentID),
      quotaID = quotaName,
      titanTradeID = titanTradeID,
      inventoryID = assignment.flatMap(_.inventoryID),
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

trait TitanEdmTradeService {
  val titanGetEdmTradesService : EdmGetTrades
}

trait TitanServices extends TitanTacticalRefData with TitanEdmTradeService
