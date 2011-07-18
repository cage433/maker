package starling.services.rpc.refdata

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import com.trafigura.edm.tradeservice.{EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.tradecapture.internal.refinedmetal.{Market, Metal}
import com.trafigura.edm.physicaltradespecs.QuotaDetail
import com.trafigura.tradecapture.internal.refinedmetal.Metal
import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradecapture.internal.refinedmetal.Market
import com.trafigura.edm.tradeservice.TradeResults
import com.trafigura.edm.trades.Trade
import com.trafigura.edm.tradeservice.TradeResult
import starling.services.StarlingInit
import com.trafigura.tradinghub.support.{ModelObject, GUID}
import java.io.{BufferedWriter, FileWriter}
import starling.services.rpc.valuation.ValuationService
import starling.services.rpc.logistics.FileUtils


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
  import starling.services.rpc.logistics.FileUtils._

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
}

// for some strange reason EDM trade service converts Neptune quota ID with prefix NEPTUNE:
case class NeptuneId(id : String) {
  def identifier : String = identifier(id)
  def identifier(ident : String) : String = ident match {
    case i : String if i != null => {
      val neptunePrefix = "NEPTUNE:"
      ident.substring(neptunePrefix.length)
    }
    case null => null
  }
}

case class FileMockedTitanServicesDataFileGenerator(titanEdmTradeService : TitanEdmTradeService, valuationService : ValuationService) {

  import org.codehaus.jettison.json.JSONObject
  import FileUtils._

  println("Starting FileMockedTitanServicesDataFileGenerator")
  valuationService.marketDataSnapshotIDs().foreach(println)
  val valuations = valuationService.valueAllQuotas()

//  valuations.tradeResults.foreach(println)

  val (_, worked) = valuations.tradeResults.values.partition({ case Right(_) => true; case Left(_) => false })

  //val tradeIds = valuations.tradeResults.collect{ case (id, Left(v)) => id }.toList
  //val trades = valuationService.getTrades(tradeIds)
  val trades = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null)
  val markets = valuationService.getFuturesMarkets.toList
  val exchanges = valuationService.getFuturesExchanges.toList

  /**
   * Write out EDM trades from trade service (that can be valued successfully) and the ref-data markets and exchanges
   *   so that the file mocked services can use canned data for tests (note this data needs moving into resources to update
   *   canned data for the tests...)
   */
  val tradesFile = "/tmp/edmTrades.json"
  val marketsFile = "/tmp/markets.json"
  val exchangesFile = "/tmp/exchanges.json"

  writeJson(tradesFile, trades)
  writeJson(marketsFile, markets)
  writeJson(exchangesFile, exchanges)

  val loadedMarkets = loadJsonValuesFromFile(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
  val loadedExchanges = loadJsonValuesFromFile(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
  val loadedTrades = loadJsonValuesFromFile(tradesFile).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade])

  loadedMarkets.foreach(println)
  loadedExchanges.foreach(println)
  println("loaded trade size = " + loadedTrades.size)

  def writeJson[T <: ModelObject with Object { def toJson() : JSONObject }](fileName : String, objects : List[T]) {
    try {
      val fStream = new FileWriter(fileName)
      val bWriter = new BufferedWriter(fStream)
      objects.foreach(obj => bWriter.write(obj.toJson().toString() + "\n" ))
      bWriter.flush()
      fStream.close()
    }
    catch {
      case ex : Exception => println("Error: " + ex.getMessage())
    }
  }
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
