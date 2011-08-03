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
import com.trafigura.tradecapture.internal.refinedmetal.{Counterparty, Metal, Market, UOM}
import com.trafigura.timer.Timer
import com.trafigura.edm.shared.types.TitanId


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
  private lazy val tacticalRefdataUomService : UOMService = new UOMServiceResourceProxy(ProxyFactory.create(classOf[UOMServiceResource], refdataServiceURL, clientExecutor))
  lazy val titanGetEdmTradesService: EdmGetTrades = new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, clientExecutor))

  lazy val edmMetalByGUID: Map[GUID, Metal] = Map[GUID, Metal]() ++ allTacticalRefDataMetals.map(e => (e.guid, e))
  lazy val futuresExchangeByID: Map[String, Market] = Map[String, Market]() ++ allTacticalRefDataExchanges.map(e => (e.code, e))
  lazy val counterpartiesByGUID: Map[GUID, Counterparty] = Map[GUID, Counterparty]() ++ allTacticalRefDataCounterparties().map(e => e.guid -> e)
  lazy val uomById : Map[Int, UOM] = Map[Int, UOM]() ++ allTacticalRefDataUoms().map(e => e.oid -> e)

  def allTacticalRefDataMetals() = tacticalRefdataMetalsService.getMetals()
  def allTacticalRefDataExchanges() = tacticalRefdataMarketsService.getMarkets()
  def allTacticalRefDataCounterparties() = tacticalRefdataCounterpartiesService.getCounterparties(true)
  def allTacticalRefDataUoms() = tacticalRefdataUomService.getUOMs()
}

/**
 * Looks like real ref-data, but really it comes from static data for testing purposes
 */
case class FileMockedTitanServices() extends TitanServices {
   
  import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
  import starling.services.rpc.FileUtils._

  val resourcePath = "/tests/valuationservice/testdata"
  val tradesFile = getClass.getResource(resourcePath + "/allEdmTrades.json.zip") // "/edmTrades.json")
  val metalsFile = getClass.getResource(resourcePath + "/metals.json")
  val exchangesFile = getClass.getResource(resourcePath + "/exchanges.json")
  val uomsFile = getClass.getResource(resourcePath + "/uoms.json")

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
    def getQuota(id : TitanId) = throw new Exception("Not implemented yet") // todo... implement
  }

  lazy val edmMetalByGUID: Map[GUID, Metal] = loadedMetals.map(m => m.guid -> m).toMap
  lazy val futuresExchangeByID: Map[String, Market] = loadedExchanges.map(e => e.code -> e).toMap
  lazy val uomById: Map[Int, UOM] = loadedUoms.map(e => e.oid -> e).toMap
  lazy val counterpartiesByGUID: Map[GUID, Counterparty] = Map[GUID, Counterparty]()

  def allTacticalRefDataFuturesMarkets() = Nil
  def allTacticalRefDataExchanges() = Nil

  import Timer._
  val loadedMetals = time(loadJsonValuesFromFileUrl(metalsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal]), t => println("took %dms to get metals".format(t)))
  val loadedExchanges = time(loadJsonValuesFromFileUrl(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market]), t => println("took %dms to get exchanges".format(t)))
  val loadedUoms = time(loadJsonValuesFromFileUrl(uomsFile).map(s => UOM.fromJson(new JSONObject(s)).asInstanceOf[UOM]), t => println("took %dms to get uom".format(t)))
  val loadedTrades = time(loadJsonValuesFromFileUrl(tradesFile, true).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade]), t => println("took %dms to get trades".format(t)))
  var tradeMap = loadedTrades.map(t => t.oid -> t).toMap

  def updateTrade(trade : EDMPhysicalTrade) {
    tradeMap = tradeMap.updated(trade.oid, trade)
  }
}

case class FileMockedTitanServicesDataFileGenerator(titanEdmTradeService : TitanEdmTradeService, valuationService : ValuationService) {

  import org.codehaus.jettison.json.JSONObject
  import FileUtils._

  val tradesFile = "/tmp/allEdmTrades.json.zip"
  val metalsFile = "/tmp/metals.json"
  val exchangesFile = "/tmp/exchanges.json"
  val uomsFile = "/tmp/uoms.json"

  println("Starting FileMockedTitanServicesDataFileGenerator")

  val metals = valuationService.getMetals.toList
  val exchanges = valuationService.getFuturesExchanges.toList
  val uoms = valuationService.getUoms.toList

  writeJson(metalsFile, metals)
  writeJson(exchangesFile, exchanges)
  writeJson(uomsFile, uoms)

  val loadedMetals = loadJsonValuesFromFile(metalsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
  loadedMetals.foreach(println)
  val loadedExchanges = loadJsonValuesFromFile(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
  loadedExchanges.foreach(println)
  val loadedUoms = loadJsonValuesFromFile(uomsFile).map(s => UOM.fromJson(new JSONObject(s)).asInstanceOf[UOM])
  loadedUoms.foreach(println)

  /**
   * get edm trades and store in mock data file
   */
  valuationService.marketDataSnapshotIDs().foreach(println)
  val valuations = valuationService.valueAllQuotas()

//  valuations.tradeResults.foreach(println)

  val (worked, failed) = valuations.tradeResults.values.partition(_ isRight)
  failed.foreach(println)
  val tradeIds = valuations.tradeResults.collect{ case (id, Right(_)) => id }.toList
  //val trades = valuationService.getTrades(tradeIds)
  val trades = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null)

  println("read %d trades ".format(trades.size))

  writeJson(tradesFile, trades, true)

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
