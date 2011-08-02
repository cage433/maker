package starling.services.rpc.refdata

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import com.trafigura.edm.tradeservice.{EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market, Metal}
import com.trafigura.tradecapture.internal.refinedmetal.Metal
import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradecapture.internal.refinedmetal.Market
import com.trafigura.edm.tradeservice.TradeResults
import com.trafigura.edm.trades.Trade
import com.trafigura.edm.tradeservice.TradeResult


/**
 * Tactical ref data, service proxies / data
 *   also includes the trademgmt EDM trade serivce, this should be refactored to  separate out at some point
 */
trait TitanTacticalRefData {
   
//  val titanGetEdmTradesService : EdmGetTrades

  val futuresMarketByGUID: Map[GUID, Metal]
  val futuresExchangeByGUID: Map[GUID, Market]

  //def allTacticalRefDataFuturesMarkets() : List[Metal]
  //def allTacticalRefDataExchanges() : List[Market]
}

trait TitanEdmTradeService {
  val titanGetEdmTradesService : EdmGetTrades
}

trait TitanServices extends TitanTacticalRefData with TitanEdmTradeService

case class DefaultTitanServices(props: Props) extends TitanServices {
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

/**
 * Looks like real ref-data, but really it comes from static data for testing purposes
 */
case class FileMockedTitanServices() extends TitanServices {
   
  import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
  import java.net.URL

  val resourcePath = "/tests/valuationservice/testdata"
  val tradesFile = getClass.getResource(resourcePath + "/edmTrades.json")
  val marketsFile = getClass.getResource(resourcePath + "/markets.json")
  val exchangesFile = getClass.getResource(resourcePath + "/exchanges.json")

  val titanGetEdmTradesService : EdmGetTrades = new EdmGetTrades {
    def getAll() : TradeResults = new TradeResults() {
      cached = true
      results = tradeMap.values.map(t => new TradeResult() {
        trade = t
        error = null
      }).toList
    }
    def getByOid(oid : Int) : Trade = tradeMap.get(oid) match {
      case Some(trade) => trade.asInstanceOf[EDMPhysicalTrade]
      case _ => throw new Exception("Trade does not exist in mock data %d".format(oid))
    }
  }

  lazy val futuresMarketByGUID: Map[GUID, Metal] = loadedMarkets.map(m => m.guid -> m).toMap
  lazy val futuresExchangeByGUID: Map[GUID, Market] = loadedExchanges.map(e => e.guid -> e).toMap

  def allTacticalRefDataFuturesMarkets() = Nil
  def allTacticalRefDataExchanges() = Nil

  val loadedMarkets = loadJsonValuesFromFileUrl(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
  val loadedExchanges = loadJsonValuesFromFileUrl(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
  val loadedTrades = loadJsonValuesFromFileUrl(tradesFile).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade])
  var tradeMap = loadedTrades.map(t => t.oid -> t).toMap

  def updateTrade(trade : EDMPhysicalTrade) {
    tradeMap = tradeMap.updated(trade.oid, trade)
  }
  
  import scala.io.Source._
  private def loadJsonValuesFromFileUrl(fileUrl : URL) : List[String] = fromURL(fileUrl).getLines.toList
  private def loadJsonValuesFromFile(file : String) : List[String] = fromFile(file).getLines.toList
}
