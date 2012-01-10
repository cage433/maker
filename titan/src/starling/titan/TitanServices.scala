package starling.titan

import com.trafigura.edm.logistics.inventory._
import com.trafigura.tradinghub.support.GUID
import com.trafigura.trademgmt.internal.refinedmetal._
import com.trafigura.edm.trademgmt.trade.EdmGetTrades
import starling.utils.{Stopwatch, Log}
import com.trafigura.services.valuation.TradeManagementCacheNotReady
import com.trafigura.edm.trademgmt.trades.{CompletedTradeState, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.common.units.TitanId
import starling.props.Props


// for some strange reason EDM trade service converts titan quota ID with prefix NEPTUNE:
case class NeptuneId(id : String) {
  import NeptuneId._
  lazy val identifier: String = id match {
    case NeptuneIdFormat(id) => id
    case other => other
  }
  def titanId : TitanId = TitanId(identifier)
}

object NeptuneId {
  val NeptuneIdFormat = "NEPTUNE:(.*)".r
  def apply(identifier : TitanId) : NeptuneId = {
    NeptuneId(Option(identifier).map(_.value).getOrElse("No Titan ID"))
  }
}

object JMXEnabler extends Log {
  import dispatch._
  import java.net.URL

  // temporary measure, until we address this properly
  def enableLogisticsAPIs(props : Props) {
    val http = new Http
    val logisticsHostURL : URL = new URL(props.LogisticsServiceLocation.value())
    val logisticsReq = :/(logisticsHostURL.getHost, logisticsHostURL.getPort) / "jmx-console" / "HtmlAdaptor"
    val postURlEncodedAttributes = "action=updateAttributes&name=trafigura.logistics%3Aname%3Dapi-properties&EnableGetInventoryByGroupCompany=True"

    def sendRequest(postAttributes: String): String =
      http(logisticsReq << (postURlEncodedAttributes, "application/x-www-form-urlencoded")  as_str)

    val result = sendRequest(postURlEncodedAttributes)
    http.shutdown()
    log.debug("HTTP Logistics JMX result: \n" + result)
  }
}

/**
 * logistics service interface
 */
object LogisticsServices {
  type EdmInventoryServiceWithGetAllInventory = EdmInventoryService with Object {
    def getAllInventoryLeaves() : List[InventoryItem]
    def getAllInventory() : LogisticsInventoryResponse
  }
}

import LogisticsServices._

trait TitanLogisticsInventoryServices extends ServiceProxy[EdmInventoryServiceWithGetAllInventory]

trait ServiceProxy[T] {
  val service : T
}

trait TitanLogisticsServices {
  val inventoryService : TitanLogisticsInventoryServices
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

trait TitanEdmTradeService extends Log {
  val titanGetEdmTradesService : EdmGetTrades

 def getTrade(id : TitanId) : EDMPhysicalTrade = {
    try {
      val trade = titanGetEdmTradesService.get(id).asInstanceOf[EDMPhysicalTrade]

      if (trade.state != CompletedTradeState) {
        log.error("fetched single trade %s and it was in an unexpected state %s".format(trade.identifier.value, trade.state))
        throw new Exception("Incorrect trade state for trade " + trade.identifier.value)
      }
      trade
    }
    catch {
      case e : Throwable => throw new ExternalTitanServiceFailed(e)
    }
  }

  def getAllCompletedPhysicalTrades() : List[EDMPhysicalTrade] = {
    try {
      val sw = new Stopwatch()
      log.info("Calling EDM Trade service")
      val edmTradeResult = titanGetEdmTradesService.getAll()

      log.info("Are EDM Trades available, cache state " + edmTradeResult.cached + ", took " + sw)
      if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady

      log.info("Got Edm Trade results, trade result count = " + edmTradeResult.results.size)

      val edmTrades = edmTradeResult.results.filter(_.error == null).map(_.trade)
        .collect({ case t : EDMPhysicalTrade if (t != null) => t }).filter(pt => pt.state == CompletedTradeState)

      val dupTrades = edmTrades.groupBy(_.identifier)
      assert(!dupTrades.exists(_._2.size > 1), "Duplicate trade Ids received from EDM trade service, ".format(dupTrades.keys.mkString(", ")))

      // temporary code, trademgmt are sending us null titan ids
      val nullIds = edmTrades.filter(_.identifier == null)
      assert(nullIds.size == 0, "%d null titan ids found - fatal error".format(nullIds.size))

      log.info("Got %d valid, completed physical trades".format(edmTrades.size))
      edmTrades
    }
    catch {
      case e : Throwable => {
        e.getCause match {
          case _ : TradeManagementCacheNotReady => {
            Log.error("Trade management cache not ready")
            Nil
          }
          case _ => throw new ExternalTitanServiceFailed(e)
        }
      }
    }
  }
}

trait TitanServices extends TitanTacticalRefData with TitanEdmTradeService

class ExternalTitanServiceFailed(cause : Throwable) extends Exception(cause)
