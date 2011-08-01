package starling.titan

import com.trafigura.tradinghub.support.GUID
import com.trafigura.edm.tradeservice.EdmGetTrades
import com.trafigura.tradecapture.internal.refinedmetal.{Counterparty, Market, Metal}
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.edm.materialspecification.CommoditySpec
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.systemofrecord.SystemOfRecord
import starling.daterange.Day
import starling.pivot.Field
import starling.trade.{Trade, TradeID, TradeAttributes}
import starling.instrument.physical.PhysicalMetalAssignment
import com.trafigura.edm.shared.types.Date
import starling.db.TitanTradeSystem
import java.lang.UnsupportedOperationException
import starling.utils.StackTraceToString
import starling.instrument.{ErrorInstrument, Costs, Tradeable}
import com.trafigura.edm.logistics.inventory._


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

  lazy val quotaNameToTradeMap : Map[String, EDMPhysicalTrade] = Map() ++ titanTradeCache.getAllTrades().flatMap{trade =>  trade.asInstanceOf[EDMPhysicalTrade].quotas.map{q => NeptuneId(q.detail.identifier).identifier -> trade}}
  lazy val quotaNameToQuotaMap : Map[String, EDMQuota] = Map() ++ titanTradeCache.getAllTrades().flatMap{trade =>  trade.asInstanceOf[EDMPhysicalTrade].quotas.map{q => NeptuneId(q.detail.identifier).identifier -> q}}

  lazy val tc = new TradeConverter(refData, quotaNameToTradeMap, quotaNameToQuotaMap)
  import tc._

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {
    
    val assignments = logisticsServices.assignmentService.service.getAllAssignments()

    val tradeErrors = assignments.map(edmAssignment => {
      try {
        f(edmAssignment)
        Right(edmAssignment.oid.toString)
      }
      catch {
        case ex : ExternalTitanServiceFailed => throw ex.getCause
        case ex : Throwable => {
          val errorInstrument = ErrorInstrument(StackTraceToString.string(ex).trim)
          val errorTrade = Trade(TradeID(edmAssignment.oid.contents, TitanTradeSystem), Day(1980, 1, 1), "Unknown", TitanTradeAttributes(edmAssignment.quotaName, "Unknown Titan ID"), errorInstrument)
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
class TradeConverter( refData : TitanTacticalRefData,
                       quotaNameToTradeMap : Map[String, EDMPhysicalTrade],
                       quotaNameToQuotaMap : Map[String, EDMQuota]) {

  /**
   * convert EDMAssignmentItem to a Starling Trade
   */
  implicit def toTrade(edmAssignment : EDMAssignment) : Trade = {

    val quotaDetail = quotaNameToQuotaMap(edmAssignment.quotaName).detail
    val edmTrade = quotaNameToTradeMap(edmAssignment.quotaName)
    val attributes : TradeAttributes = TitanTradeAttributes(edmTrade.identifier, edmAssignment.quotaName)

    import EDMConversions._

    val metal : Metal = refData.edmMetalByGUID(quotaDetail.deliverySpecs.head.materialSpec.asInstanceOf[CommoditySpec].commodity)

    val priceSpecConverter = EDMPricingSpecConverter(metal, refData.futuresExchangeByGUID)

    val deliveryQuantity = quotaDetail.deliverySpecs.map{ds => fromTitanQuantity(ds.quantity)}.sum

    val deliveryDay = Day.fromLocalDate(quotaDetail.deliverySpecs.head.schedule.asInstanceOf[Date].datex)
    val tradeable : Tradeable = PhysicalMetalAssignment(
      metal.name, //: String,
      deliveryQuantity, // : Quantity,
      deliveryDay, // deliveryDay : Day,
      priceSpecConverter.fromEdmPricingSpec(deliveryDay, deliveryQuantity, quotaDetail.pricingSpec))

    val costs : List[Costs] = Nil // todo

    if (edmTrade.submitted == null) {
      println("NULL DATE  " + edmTrade.tradeId)
    }

    Trade(
      TradeID(edmAssignment.oid.contents, TitanTradeSystem), // tradeID: TradeID,
//      Day.fromLocalDate(edmTrade.contractDraftedDate), // tradeDay: Day,
      Day.fromLocalDate(edmTrade.submitted.toLocalDate), // tradeDay: Day,
      refData.counterpartiesByGUID(edmTrade.counterparty.counterparty).name,  // counterParty: String,
      attributes, // @transient attributes: TradeAttributes,
      tradeable, // tradeable: Tradeable,
      costs // costs: List[Costs] = Nil)
      )
  }

/** not needed?
  implicit def toTrade(edmTrade : EDMPhysicalTrade)(implicit refData : TitanTacticalRefData) : Trade = {

    val attributes : TradeAttributes = TitanTradeAttributes(edmTrade.identifier, "todo...")

    val tradeable : Tradeable = null /* PhysicalMetalAssignment(
      refData.futuresMarketByGUID(edmTrade.quotas.head.detail.deliverySpecs.head.materialSpec).name, //: String,
      edmTrade.quotas.map(_.detail.pricingSpec.quantity.amount.getOrElse(0)).sum // : Quantity,
      deliveryDay : Day,
      pricingSpec : TitanPricingSpec,
      quotaID : String,
      titanTradeID : String) */

    val costs : List[Costs] = Nil

    Trade(
      TradeID(edmTrade.identifier, new TradeSystem("TitanTrades", "-") {}), // tradeID: TradeID,
      Day.fromLocalDate(edmTrade.contractDraftedDate), // tradeDay: Day,
      refData.counterpartiesByGUID(edmTrade.counterparty.counterparty).name,  // counterParty: String,
      attributes, // @transient attributes: TradeAttributes,
      tradeable, // tradeable: Tradeable,
      costs // costs: List[Costs] = Nil)
      )
  } */
}

case class TitanTradeAttributes(quotaID : String, titanTradeID : String) extends TradeAttributes {
  import TitanTradeStore._
  require(quotaID != null)
  require(titanTradeID != null)
  def details = Map(
    quotaID_str -> quotaID,
    PhysicalMetalAssignment.titanTradeIDLabel -> titanTradeID
  )

  override def createFieldValues = Map(
    Field(quotaID_str) -> quotaID,
    Field(PhysicalMetalAssignment.titanTradeIDLabel) -> titanTradeID
  )
}


/**
 * logistics service interface
 */
object LogisticsServices {
  type EdmAssignmentServiceWithGetAllAssignments = EdmAssignmentService with Object { def getAllAssignments() : List[EDMAssignment] }
  type EdmInventoryServiceWithGetAllInventory = EdmInventoryService with Object { def getAllInventoryLeaves() : List[EDMInventoryItem] }
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
  protected var tradeMap: Map[String, EDMPhysicalTrade]
  protected var quotaIDToTradeIDMap: Map[String, String]
  def getTrade(id: String): EDMPhysicalTrade
  def getAllTrades(): List[EDMPhysicalTrade]
  def removeTrade(id : String) {
    tradeMap = tradeMap - id
    quotaIDToTradeIDMap = quotaIDToTradeIDMap.filter{ case (_, value) => value != id}
  }

  def addTrade(id : String) {
    tradeMap += id -> getTrade(id)
    addTradeQuotas(id)
  }

  def addTradeQuotas(id : String) {
    val trade = tradeMap(id)
    quotaIDToTradeIDMap ++= trade.quotas.map{quota => (quota.detail.identifier, id)}
  }

  def tradeIDFromQuotaID(quotaID: String): String
}

/**
 * Tactical ref data, service proxies / data
 *   also includes the trademgmt EDM trade serivce, this should be refactored to  separate out at some point
 */
trait TitanTacticalRefData {

  val edmMetalByGUID: Map[GUID, Metal]
  val futuresExchangeByGUID: Map[GUID, Market]
  val counterpartiesByGUID: Map[GUID, Counterparty]
}

trait TitanEdmTradeService {
  val titanGetEdmTradesService : EdmGetTrades
}

trait TitanServices extends TitanTacticalRefData with TitanEdmTradeService
