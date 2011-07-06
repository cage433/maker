package starling.services.rpc.refdata

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import com.trafigura.edm.tradeservice.{EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market, Metal}
import com.trafigura.edm.physicaltradespecs.QuotaDetail
import com.trafigura.tradecapture.internal.refinedmetal.Metal
import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradecapture.internal.refinedmetal.Market
import com.trafigura.edm.tradeservice.TradeResults
import com.trafigura.edm.trades.Trade
import com.trafigura.edm.tradeservice.TradeResult


/**
 * Tactical ref data, service proxies / data
 */
trait TitanTacticalRefData {
   
  val titanGetEdmTradesService : EdmGetTrades

  val futuresMarketByGUID: Map[GUID, Metal]
  val futuresExchangeByGUID: Map[GUID, Market]

  def allTacticalRefDataFuturesMarkets() : List[Metal]
  def allTacticalRefDataExchanges() : List[Market]
}

case class DefaultTitanTacticalRefData(props: Props) extends TitanTacticalRefData {
  val rmetadminuser = props.ServiceInternalAdminUser()
  val tradeServiceURL = props.EdmTradeServiceUrl()
  val refdataServiceURL = props.TacticalRefDataServiceUrl()

  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  private lazy val tacticalRefdataMetalsService: MetalService = new MetalServiceResourceProxy(ProxyFactory.create(classOf[MetalServiceResource], refdataServiceURL, clientExecutor))
  private lazy val tacticalRefdataMarketsService: MarketService = new MarketServiceResourceProxy(ProxyFactory.create(classOf[MarketServiceResource], refdataServiceURL, clientExecutor))
  lazy val titanGetEdmTradesService: EdmGetTrades = new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, clientExecutor))

  lazy val futuresMarketByGUID: Map[GUID, Metal] = Map[GUID, Metal]() ++ allTacticalRefDataFuturesMarkets.map(e => (e.guid, e))
  lazy val futuresExchangeByGUID: Map[GUID, Market] = Map[GUID, Market]() ++ allTacticalRefDataExchanges.map(e => (e.guid, e))

  def allTacticalRefDataFuturesMarkets() = tacticalRefdataMetalsService.getMetals()
  def allTacticalRefDataExchanges() = tacticalRefdataMarketsService.getMarkets()
}

case class FileMockedTitanTacticalRefData() extends TitanTacticalRefData {
   
  import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
  val tradesFile = "/tmp/edmTrades.json"
  val marketsFile = "/tmp/markets.json"
  val exchangesFile = "/tmp/exchanges.json"

  val titanGetEdmTradesService : EdmGetTrades = new EdmGetTrades {
    def getAll() : TradeResults = new TradeResults() {
      cached = true
      results = loadedTrades.map(t => new TradeResult() {
        trade = t
        error = null
      })
    }
    def getByOid(oid : Int) : Trade = loadedTrades.find(t => t.oid == oid).asInstanceOf[EDMPhysicalTrade]
  }
  lazy val futuresMarketByGUID: Map[GUID, Metal] = loadedMarkets.map(m => m.guid -> m).toMap
  lazy val futuresExchangeByGUID: Map[GUID, Market] = loadedExchanges.map(e => e.guid -> e).toMap

  def allTacticalRefDataFuturesMarkets() = Nil
  def allTacticalRefDataExchanges() = Nil

  val loadedMarkets = loadJsonValuesFromFile(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
  val loadedExchanges = loadJsonValuesFromFile(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
  val loadedTrades = loadJsonValuesFromFile(tradesFile).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade])
  
  import scala.io.Source._
  private def loadJsonValuesFromFile(fileName : String) : List[String] = 
    fromFile(fileName).getLines.toList
}

