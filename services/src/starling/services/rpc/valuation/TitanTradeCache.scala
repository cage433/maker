package starling.services.rpc.valuation

import starling.props.Props
import com.trafigura.services.valuation._
import starling.services.rpc.refdata._
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import java.lang.Exception
import starling.titan._
import com.trafigura.edm.shared.types.TitanId
import starling.utils.{Log, Stopwatch}
import starling.utils.conversions.RichMapWithErrors._


/**
 * Trade cache provide trade map lookup by trade id and also a quota id to trade map lookup
 */


case class DefaultTitanTradeCache(props : Props) extends TitanTradeCache with Log {
  protected var tradeMap = Map[TitanId, EDMPhysicalTrade]().withException()
  protected var quotaIDToTradeIDMap = Map[String, TitanId]().withException()

  private lazy val titanTradesService = new DefaultTitanServices(props).titanGetEdmTradesService
  private def getAll() = try {
      titanTradesService.getAll()
  } catch {
    case e : Throwable => { log.error("Error getting Titan EDM trades ", e); throw new ExternalTitanServiceFailed(e) }
  }

  private def getById(id : TitanId) = try {
      titanTradesService.get(id)
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }

  /*
    Read all trades from Titan and blast our cache
  */
  def updateTradeMap() {
    val sw = new Stopwatch()
    val edmTradeResult = getAll()
    log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)

    val validTrades = edmTradeResult.results.filter(tr => tr.trade != null).map(_.trade.asInstanceOf[EDMPhysicalTrade])

    // temporary code, trademgmt are sending us null titan ids
    val (nullIds, validIds) = validTrades.span(_.titanId == null)
    if (nullIds.size > 0) {
      log.error("Null Titan trade IDs found!")
      log.error("null ids \n%s\n%s".format(nullIds, validIds))
      //assert(false, "Null titan ids found - fatal error")
    }
    // todo, line below, we really should filter for completed trades only??
    tradeMap = validTrades/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.titanId, t)).toMap.withException()
    tradeMap.keySet.foreach(addTradeQuotas)
  }

  def getAllTrades(): List[EDMPhysicalTrade] = {
    if (tradeMap.size > 0) {
      tradeMap.values.toList
    }
    else {
      updateTradeMap()
      tradeMap.values.toList
    }
  }

  def getTrade(id: TitanId): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = getById(id)
      tradeMap += trade.titanId -> trade.asInstanceOf[EDMPhysicalTrade]
      addTradeQuotas(id)
      tradeMap(id)
    }
  }

  def tradeIDFromQuotaID(quotaID: String): TitanId = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }
}


/**
 * Trade cache using supplied ref data
 */
case class TitanTradeServiceBasedTradeCache(titanTradesService : TitanTradeService) extends TitanTradeCache {

  protected var tradeMap = Map[TitanId, EDMPhysicalTrade]().withException()
  protected var quotaIDToTradeIDMap = Map[String, TitanId]().withException()

  // Read all trades from Titan and blast our cache
  def updateTradeMap() {
    tradeMap = titanTradesService.getAllTrades()/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.titanId, t)).toMap
    tradeMap.keySet.foreach(addTradeQuotas)
  }

  def getAllTrades(): List[EDMPhysicalTrade] = {
    if (tradeMap.size > 0) {
      tradeMap.values.toList
    }
    else {
      updateTradeMap()
      tradeMap.values.toList
    }
  }

  def getTrade(id: TitanId): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = titanTradesService.getTrade(id)
      tradeMap += trade.titanId -> trade
      tradeMap(id)
    }
  }

  def tradeIDFromQuotaID(quotaID: String): TitanId = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }
}

