package starling.services.rpc.valuation

import com.trafigura.edm.shared.types.TitanId
import starling.titan.TitanServices
import starling.utils.{Stopwatch, Log}
import com.trafigura.services.valuation.TradeManagementCacheNotReady
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}


trait TitanTradeService {
  def getTrade(id : TitanId) : EDMPhysicalTrade
  def getAllTrades() : List[EDMPhysicalTrade]
}

class DefaultTitanTradeService(titanServices : TitanServices) extends TitanTradeService with Log {

  def getTrade(id : TitanId) : EDMPhysicalTrade = {
    titanServices.titanGetEdmTradesService.get(id).asInstanceOf[EDMPhysicalTrade]
  }

  def getAllTrades() : List[EDMPhysicalTrade] = {
    val sw = new Stopwatch()
    val edmTradeResult = titanServices.titanGetEdmTradesService.getAll()
    log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
    edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])
  }
}

