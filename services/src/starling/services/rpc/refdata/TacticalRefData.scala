package starling.services.rpc.refdata

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import com.trafigura.edm.tradeservice.{EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import com.trafigura.tradinghub.support.GUID
import com.trafigura.tradecapture.internal.refinedmetal.{Market, Metal}
import com.trafigura.edm.physicaltradespecs.QuotaDetail


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

