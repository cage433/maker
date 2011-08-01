package starling.services.rpc.refdata

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import com.trafigura.edm.tradeservice.{EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import org.codehaus.jettison.json.JSONObject
import com.trafigura.edm.tradeservice.TradeResults
import com.trafigura.edm.trades.Trade
import com.trafigura.edm.tradeservice.TradeResult
import starling.services.StarlingInit
import com.trafigura.tradinghub.support.GUID
import starling.services.rpc.valuation.ValuationService
import starling.services.rpc.FileUtils
import starling.titan.{TitanEdmTradeService, TitanServices}
import com.trafigura.tradecapture.internal.refinedmetal.{Counterparty, Metal, Market}
import com.trafigura.timer.Timer


/**
 * Tactical ref data, service proxies / data
 *   also includes the trademgmt EDM trade serivce, this should be refactored to  separate out at some point
 */
case class DefaultTitanServices(props: Props) extends TitanServices {
  val rmetadminuser = props.ServiceInternalAdminUser()
  val tradeServiceURL = props.EdmTradeServiceUrl()
  val refdataServiceURL = props.TacticalRefDataServiceUrl()

  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  private lazy val tacticalRefdataMetalsService: MetalService = new MetalServiceResourceProxy(ProxyFactory.create(classOf[MetalServiceResource], refdataServiceURL, clientExecutor))
  private lazy val tacticalRefdataMarketsService: MarketService = new MarketServiceResourceProxy(ProxyFactory.create(classOf[MarketServiceResource], refdataServiceURL, clientExecutor))
  private lazy val tacticalRefdataCounterpartiesService : CounterpartyService = new CounterpartyServiceResourceProxy(ProxyFactory.create(classOf[CounterpartyServiceResource], refdataServiceURL, clientExecutor))
  lazy val titanGetEdmTradesService: EdmGetTrades = new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, clientExecutor))

  lazy val edmMetalByGUID: Map[GUID, Metal] = Map[GUID, Metal]() ++ allTacticalRefDataFuturesMarkets.map(e => (e.guid, e))
  lazy val futuresExchangeByGUID: Map[GUID, Market] = Map[GUID, Market]() ++ allTacticalRefDataExchanges.map(e => (e.guid, e))
  lazy val counterpartiesByGUID: Map[GUID, Counterparty] = Map[GUID, Counterparty]() ++ allTacticalRefDataCounterparties().map(e => e.guid -> e)

  def allTacticalRefDataFuturesMarkets() = tacticalRefdataMetalsService.getMetals()
  def allTacticalRefDataExchanges() = tacticalRefdataMarketsService.getMarkets()
  def allTacticalRefDataCounterparties() = tacticalRefdataCounterpartiesService.getCounterparties(true)
}

/**
 * Looks like real ref-data, but really it comes from static data for testing purposes
 */
case class FileMockedTitanServices() extends TitanServices {
   
  import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
  import starling.services.rpc.FileUtils._

  val resourcePath = "/tests/valuationservice/testdata"
  val tradesFile = getClass.getResource(resourcePath + "/allEdmTrades.json.zip") // "/edmTrades.json")
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

  lazy val edmMetalByGUID: Map[GUID, Metal] = loadedMarkets.map(m => m.guid -> m).toMap
  lazy val futuresExchangeByGUID: Map[GUID, Market] = loadedExchanges.map(e => e.guid -> e).toMap
  lazy val counterpartiesByGUID: Map[GUID, Counterparty] = Map[GUID, Counterparty]()

  def allTacticalRefDataFuturesMarkets() = Nil
  def allTacticalRefDataExchanges() = Nil

  import Timer._
  val loadedMarkets = time(loadJsonValuesFromFileUrl(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal]), t => println("took %dms to get markets".format(t)))
  val loadedExchanges = time(loadJsonValuesFromFileUrl(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market]), t => println("took %dms to get exchanges".format(t)))
  val loadedTrades = time(loadJsonValuesFromFileUrl(tradesFile, true).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade]), t => println("took %dms to get trades".format(t)))
  var tradeMap = loadedTrades.map(t => t.oid -> t).toMap

  def updateTrade(trade : EDMPhysicalTrade) {
    tradeMap = tradeMap.updated(trade.oid, trade)
  }
}

case class FileMockedTitanServicesDataFileGenerator(titanEdmTradeService : TitanEdmTradeService, valuationService : ValuationService) {

  import org.codehaus.jettison.json.JSONObject
  import FileUtils._

  println("Starting FileMockedTitanServicesDataFileGenerator")
  valuationService.marketDataSnapshotIDs().foreach(println)
  val valuations = valuationService.valueAllQuotas()

//  valuations.tradeResults.foreach(println)

  val (worked, failed) = valuations.tradeResults.values.partition(_ isRight)
  failed.foreach(println)
  val tradeIds = valuations.tradeResults.collect{ case (id, Right(_)) => id }.toList
  val trades = valuationService.getTrades(tradeIds)
  //val trades = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null)
  val markets = valuationService.getFuturesMarkets.toList
  val exchanges = valuationService.getFuturesExchanges.toList

  println("read %d trades ".format(trades.size))

  /**
   * Write out EDM trades from trade service (that can be valued successfully) and the ref-data markets and exchanges
   *   so that the file mocked services can use canned data for tests (note this data needs moving into resources to update
   *   canned data for the tests...)
   */
  val tradesFile = "/tmp/allEdmTrades.json.zip"
  val marketsFile = "/tmp/markets.json"
  val exchangesFile = "/tmp/exchanges.json"

  writeJson(tradesFile, trades, true)
  writeJson(marketsFile, markets)
  writeJson(exchangesFile, exchanges)

  val loadedMarkets = loadJsonValuesFromFile(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
  loadedMarkets.foreach(println)
  val loadedExchanges = loadJsonValuesFromFile(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
  loadedExchanges.foreach(println)
  val loadedTrades = loadJsonValuesFromFile(tradesFile, true).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade])
  println("loaded %d trades = ".format(loadedTrades.size))
}

object RefDataServices {

  def main(args : Array[String]) {
    println("running main for tactical ref data services")
    val server = StarlingInit.devInstance
    val edmTradeService = server.titanServices
    val valuationService = server.valuationService
    FileMockedTitanServicesDataFileGenerator(edmTradeService, valuationService)
    server.stop
  }
}
