package starling.services.rpc.valuation

import starling.titan.TitanServices
import starling.utils.{Stopwatch, Log}
import com.trafigura.services.valuation.TradeManagementCacheNotReady
import com.trafigura.edm.trademgmt.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.titan.ExternalTitanServiceFailed
import com.trafigura.edm.common.units.TitanId


/**
 * Simple wrapper around the Titan EDM Trade service

trait TitanTradeService {
  def getTrade(id : TitanId) : EDMPhysicalTrade
  def getAllTrades() : List[EDMPhysicalTrade]
}

class DefaultTitanTradeService(titanServices : TitanServices) extends TitanTradeService with Log {

  def getTrade(id : TitanId) : EDMPhysicalTrade = {
    try {
      titanServices.titanGetEdmTradesService.get(id).asInstanceOf[EDMPhysicalTrade]
    }
    catch {
      case e : Throwable => throw new ExternalTitanServiceFailed(e)
    } 
  }

  def getAllTrades() : List[EDMPhysicalTrade] = {
    try {
      val sw = new Stopwatch()
      val edmTradeResult = titanServices.titanGetEdmTradesService.getAll()
      log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
      if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
      log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
      val edmTrades = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])

      // temporary code, trademgmt are sending us null titan ids
      val (nullIds, validIds) = edmTrades.span(_.identifier == null)
      if (nullIds.size > 0) {
        log.error("Null Titan trade IDs found!\n" + ("null ids \n%s\n%s".format(nullIds, validIds)))
        //assert(false, "Null titan ids found - fatal error")
      }
      edmTrades
    }
    catch {
      case e : Throwable => throw new ExternalTitanServiceFailed(e)
    }
  }
}

*/
