package starling.titan

import com.trafigura.tradinghub.support.GUID
import com.trafigura.edm.tradeservice.EdmGetTrades
import com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.systemofrecord.SystemOfRecord
import starling.daterange.Day
import starling.daterange.Day._
import starling.pivot.Field
import starling.trade.{Trade, TradeID, TradeAttributes}
import starling.instrument.physical.PhysicalMetalAssignment
import starling.db.TitanTradeSystem
import java.lang.UnsupportedOperationException
import starling.utils.StackTraceToString
import starling.instrument.{ErrorInstrument, Costs, Tradeable}
import com.trafigura.edm.logistics.inventory._
import com.trafigura.edm.shared.types.{TitanId, Date, DateSpec, PercentageTolerance}
import starling.quantity.Percentage
import org.joda.time.LocalDate
import com.trafigura.tradecapture.internal.refinedmetal._

class ExternalTitanServiceFailed(cause : Throwable) extends Exception(cause)

// for some strange reason EDM trade service converts Neptune quota ID with prefix NEPTUNE:
case class NeptuneId(id : String) {
  def identifier : String = identifier(id)
  def identifier(ident : String) : String = ident match {
    case i : String if i != null => {
      val neptunePrefix = "NEPTUNE:"
      ident.substring(neptunePrefix.length)
    }
    case null => null
  }
}

class TitanSystemOfRecord(
  titanTradeCache : TitanTradeCache,
  refData : TitanTacticalRefData,
  logisticsServices : TitanLogisticsServices) extends SystemOfRecord {

  lazy val quotaNameToTradeMap : Map[String, EDMPhysicalTrade] = Map() ++ titanTradeCache.getAllTrades().flatMap{trade =>  trade.asInstanceOf[EDMPhysicalTrade].quotas.map{q => NeptuneId(q.detail.identifier.value).identifier -> trade}}
  lazy val quotaNameToQuotaMap : Map[String, PhysicalTradeQuota] = Map() ++ titanTradeCache.getAllTrades().flatMap{trade =>  trade.asInstanceOf[EDMPhysicalTrade].quotas.map{q => NeptuneId(q.detail.identifier.value).identifier -> q}}

  lazy val tc = new TradeConverter(refData, quotaNameToTradeMap, quotaNameToQuotaMap)
  import tc._

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {
    
    val assignments = logisticsServices.assignmentService.service.getAllAssignments()

    val tradeErrors = assignments.map(edmAssignment => {
      try {
        f(edmAssignment)
        Right(edmAssignment.oid.contents.toString)
      }
      catch {
        case ex : ExternalTitanServiceFailed => throw ex.getCause
        case ex : Throwable => {
          val errorInstrument = ErrorInstrument(StackTraceToString.string(ex).trim)
          val dummyDate = Day(1980, 1, 1)

          val errorTrade = Trade(TradeID(edmAssignment.oid.contents, TitanTradeSystem), dummyDate, "Unknown", TitanTradeAttributes.errorAttributes(edmAssignment, quotaNameToTradeMap.get(edmAssignment.quotaName)), errorInstrument)
          f(errorTrade)
          Left(if (ex.getMessage == null) "Null Error Message" else ex.getMessage)
        }
      }
    }).collect({ case Left(err) => err }).distinct
    
    (tradeErrors.size, tradeErrors.toSet)
  }

  def trade(id: String)(f: (Trade) => Unit) {
    val edmAssignment = logisticsServices.assignmentService.service.getAssignmentById(id.toInt)
    val tradeAssignment : Trade = tc.toTrade(edmAssignment)
    f(tradeAssignment)
  }

  protected def readers = throw new UnsupportedOperationException()
}

/**
 * EDM to Starling Trade conversions
 */
class TradeConverter(refData : TitanTacticalRefData,
                     quotaNameToTradeMap : Map[String, EDMPhysicalTrade],
                     quotaNameToQuotaMap : Map[String, PhysicalTradeQuota]) {

  /**
   * convert EDMAssignmentItem to a Starling Trade
   */
  implicit def toTrade(edmAssignment : EDMAssignment) : Trade = {

    val quotaDetail = quotaNameToQuotaMap(edmAssignment.quotaName).detail
    require(quotaDetail.deliverySpecs.size == 1, "Require exactly one delivery spec")

    val edmTrade = quotaNameToTradeMap(edmAssignment.quotaName)
    val groupCompany = refData.groupCompaniesByGUID(edmTrade.groupCompany).name
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
    val assignmentId = edmAssignment.oid.contents
    val quotaId = edmAssignment.quotaName

    val attributes : TradeAttributes = TitanTradeAttributes(
      assignmentId.toString,
      quotaId,
      Option(edmTrade.titanId.value).getOrElse("NULL TITAN TRADE ID"),
      edmAssignment.inventoryId.toString,
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

    val deliveryQuantity = quotaDetail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum

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
      TradeID(assignmentId, TitanTradeSystem),
      Day.fromLocalDate(edmTrade.submitted.toLocalDate),
      counterparty,
      attributes, 
      tradeable, 
      costs
      )
  }
}

case class TitanTradeAttributes(
  assignmentID : String,
  quotaID : String,
  titanTradeID : String,
  inventoryID : String,
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
  import TitanTradeStore._
  require(quotaID != null)
  require(titanTradeID != null)
  def details = Map(
    quotaID_str -> quotaID,
    titanTradeID_str -> titanTradeID,
    assignmentID_str -> assignmentID,
    inventoryID_str -> inventoryID,
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
  )

  override def createFieldValues = details.map{
    case (k, v) => Field(k) -> v
  }
  
}

object TitanTradeAttributes{
  def errorAttributes(edmAssignment : EDMAssignment, edmTrade : Option[EDMPhysicalTrade]) = {
    val dummyDate = Day(1980, 1, 1)
    val titanTradeID = edmTrade match {
      case Some(trade) => trade.titanId.value
      case None => "Unknown"
    }

    TitanTradeAttributes(
      assignmentID = edmAssignment.oid.contents.toString,
      quotaID = edmAssignment.quotaName,
      titanTradeID = titanTradeID,
      inventoryID = edmAssignment.inventoryId.toString,
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
