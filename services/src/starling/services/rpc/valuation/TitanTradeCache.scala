package starling.services.rpc.valuation

import starling.props.Props
import com.trafigura.services.valuation._
import starling.services.rpc.refdata._
import java.lang.Exception
import starling.titan._
import com.trafigura.edm.shared.types.TitanId
import starling.utils.{Log, Stopwatch}
import starling.utils.conversions.RichMapWithErrors._
import com.trafigura.edm.trades.{CompletedTradeState, TradeStateEnum, Trade, PhysicalTrade => EDMPhysicalTrade}

/**
 * Trade cache provide trade map lookup by trade id and also a quota id to trade map lookup
 */
case class DefaultTitanTradeCache(props : Props) extends TitanTradeCache with Log {
  protected var tradeMap = Map[TitanId, EDMPhysicalTrade]().withException()
  protected var quotaIDToTradeIDMap = Map[String, TitanId]().withException()

  private lazy val titanServices = new DefaultTitanServices(props)
  private lazy val titanTradeService : TitanTradeService = new DefaultTitanTradeService(titanServices)

  private def getAll() = titanTradeService.getAllTrades()
  private def getById(id : TitanId) = titanTradeService.getTrade(id)

  private var cacheLoaded = false

  /**
   * Read all trades from Titan and blast our cache
   */
  def updateTradeMap() {

    val edmTrades = getAll() // get all edm physical trades from the underlying service
    cacheLoaded = true

    // we only want to operate on completed trades (at this time)
    tradeMap = edmTrades.filter(pt => pt.state == CompletedTradeState).map(t => (t.titanId, t)).toMap.withException()
    tradeMap.keySet.foreach(addTradeQuotas)
  }

  def getAllTrades(): List[EDMPhysicalTrade] = {
    if (cacheLoaded) {
      tradeMap.values.toList
    }
    else { // if we've not yet cached anything, get everything to start with
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
