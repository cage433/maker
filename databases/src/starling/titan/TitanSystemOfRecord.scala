package starling.titan

import starling.systemofrecord.SystemOfRecord
import com.trafigura.edm.logistics.inventory.{EDMInventoryItem, EdmInventoryService, EDMAssignmentItem, EdmAssignmentService}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.daterange.Day
import starling.instrument.{Costs, Tradeable}
import starling.trade.{TradeSystem, TradeAttributes, TradeID, Trade}
import com.trafigura.tradinghub.support.GUID
import com.trafigura.edm.tradeservice.EdmGetTrades
import com.trafigura.tradecapture.internal.refinedmetal.{Counterparty, Market, Metal}
import starling.eai.TreeID
import starling.pivot.Field._
import starling.pivot.Field
import starling.quantity.Quantity
import starling.instrument.physical.{TitanPricingSpec, PhysicalMetalAssignment}
import com.trafigura.edm.physicaltradespecs.EDMQuota

class TitanSystemOfRecord(
  titanTradeCache : TitanTradeCache,
  refData : TitanTacticalRefData,
  //titanTradeService : TitanTradeService,
  logisticsServices : TitanLogisticsServices) extends SystemOfRecord {

  import TradeConverter._
  implicit val rd = refData
  implicit val quotaNameToTradeMap : Map[String, EDMPhysicalTrade] = Map() // build a map from quota name to trade
  implicit val quotaNameToQuotaMap : Map[String, EDMQuota] = Map() // build a map from quota name to quota

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = {

    //val edmTrades = titanTradeCache.getAllTrades()
    val assignments = logisticsServices.assignmentService.service.getAllAssignments()
    
    val tradeResults = assignments.map(edmAssignment => {
      try {
        val tradeAssignment : Trade = edmAssignment
        f(tradeAssignment)
        Right(edmAssignment.oid.toString)
      }
      catch {
        case ex : Exception => Left(ex.getMessage)
      }
    }).collect({ case Left(err) => err }).distinct
    
    (tradeResults.size, tradeResults.toSet)
  }

  def trade(id: String)(f: (Trade) => Unit) : Unit = {
    val edmAssignment = logisticsServices.assignmentService.service.getAssignmentById(id.toInt)
    val tradeAssignment : Trade = edmAssignment
    f(tradeAssignment)
  }

  protected def readers = null
}

/**
 * EDM to Starling Trade conversions
 */
object TradeConverter {
  //import starling.services.EDMConversions._

  /**
   * Implicit conversion from EDMAssignmentItem to a Starling Trade
   */
  implicit def toTrade(edmAssignment : EDMAssignmentItem)
                      (implicit refData : TitanTacticalRefData,
                       quotaNameToTradeMap : Map[String, EDMPhysicalTrade],
                       quotaNameToQuotaMap : Map[String, EDMQuota]) : Trade = {

    val quotaDetail = quotaNameToQuotaMap(edmAssignment.quotaName).detail
    val edmTrade = quotaNameToTradeMap(edmAssignment.quotaName)
    val attributes : TradeAttributes = TitanTradeAttributes(edmTrade.identifier, edmAssignment.quotaName)

    // todo... create a real tradeable
    val tradeable : Tradeable = null

/*
      val tradeable : Tradeable = PhysicalMetalAssignment(
      refData.futuresMarketByGUID(quotaDetail.deliverySpecs.head.materialSpec.commodity).name, //: String,
      quotaDetail.pricingSpec.quantity, // : Quantity,
      Day.fromLocalDate(quotaDetail.expectedSales), // deliveryDay : Day,
      quotaDetail.pricingSpec, //: TitanPricingSpec,
      edmAssignment.quotaName, // : String,
      edmTrade.identifier) // : String)
*/
    val costs : List[Costs] = Nil // todo

    Trade(
      TradeID(edmTrade.tradeId, new TradeSystem("TitanTrades", "-") {}), // tradeID: TradeID,
      Day.fromLocalDate(edmTrade.contractDraftedDate), // tradeDay: Day,
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

object TitanTradeStore {
  val quotaID_str = "Quota ID"
  val tradeID_str = "Trade ID"
}

case class TitanTradeAttributes(quotaID : String, tradeID : String) extends TradeAttributes {
  import TitanTradeStore._
  require(quotaID != null)
  require(tradeID != null)
  def details = Map(
    quotaID_str -> quotaID,
    tradeID_str -> tradeID
  )

  override def createFieldValues = Map(
    Field(quotaID_str) -> quotaID,
    Field(tradeID_str) -> tradeID
  )
}


/**
 * logistics service interface
 */
object LogisticsServices {
  type EdmAssignmentServiceWithGetAllAssignments = EdmAssignmentService with Object { def getAllAssignments() : List[EDMAssignmentItem] }
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

//  val titanGetEdmTradesService : EdmGetTrades

  val futuresMarketByGUID: Map[GUID, Metal]
  val futuresExchangeByGUID: Map[GUID, Market]
  val counterpartiesByGUID: Map[GUID, Counterparty]

  //def allTacticalRefDataFuturesMarkets() : List[Metal]
  //def allTacticalRefDataExchanges() : List[Market]
}

trait TitanEdmTradeService {
  val titanGetEdmTradesService : EdmGetTrades
}

trait TitanServices extends TitanTacticalRefData with TitanEdmTradeService
