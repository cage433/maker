package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata._
import scala.collection.JavaConversions._

import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.{PriceFixingsHistoryDataType, MarketDataTypes}
import starling.pivot.model.PivotTableModel
import starling.pivot._
import starling.services.Server
import starling.utils.ImplicitConversions._
import PriceFixingsHistoryDataType._
import starling.daterange.Day
import starling.edm.EDMConversions
import com.trafigura.edm.shared.types.{Quantity => EDMQuantity}
import starling.quantity.{UOM, Quantity}
import com.trafigura.services.security.IProvideSecurityContext
import org.jboss.resteasy.client.{ClientExecutor, ProxyFactory}
import com.trafigura.services.referencedata.ReferenceData
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import com.trafigura.edm.physicaltradespecs.{PhysicalTradeSpec, QuotaDetail, EDMQuota}
import javax.management.remote.rmi._RMIConnection_Stub
import com.trafigura.edm.trades.PhysicalTrade
import com.trafigura.tradecapture.internal.refinedmetal.{Market, Metal}
import starling.props.{Props, PropsHelper}

import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice.{TranslationsServiceResource, TranslationsServiceResourceProxy}
import com.trafigura.services.security.ComponentTestClientExecutor
import com.trafigura.services.security._
import com.trafigura.timer.Timer._
import java.lang.Thread
import java.io._
import xml.Source
import starling.curves.{Environment, NullAtomicEnvironment}
import org.apache.commons.io.FileUtils
import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradinghub.support.{JSONConversions, GUID, ServiceFilter}
import starling.utils.{Stopwatch, StarlingXStream, Log}
import org.apache.commons.codec.net.QCodec
import com.trafigura.edm.tradeservice.{TradeResults, EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import starling.services.rpc.valuation.{InvalidPricingSpecException, QuotaValuer}

/**
 * Generic Market data service, covering all market data
 */
class MarketDataServiceRPC(marketDataStore: MarketDataStore, val props : Props) extends MarketDataService {
  implicit def enrichMarketDataRequestParameters(parameters: MarketDataRequestParameters) = new {
    def filterExchange(exchangeNames: String*) = addFilters(MarketDataFilter(exchangeField.name, exchangeNames.toList))
    def addFilters(filter: MarketDataFilter*) = parameters.filters = parameters.filters ::: filter.toList
  }

  /*
   * a generic (untyped) market data service (unsupported)
   */
  def marketData(parameters: MarketDataRequestParameters): MarketDataResponse = try {
    Log.info("MarketDataServiceRPC called with parameters " + parameters)
    parameters.notNull("Missing parameters")

    val selection = MarketDataSelection(Some(PricingGroup.fromName(parameters.pricingGroup)))
    val version   = parameters.version.getOrElse(marketDataStore.latest(selection))
    val pivot     = marketDataStore.pivot(MarketDataIdentifier(selection, version), MarketDataTypes.fromName(parameters.dataType))
    val filters   = parameters.filters.map(filter => pivot.parseFilter(Field(filter.name), filter.values))
    val pfs       = PivotFieldsState(fields(parameters.measures), fields(parameters.rows), fields(parameters.columns), filters)
    val data      = PivotTableModel.createPivotTableData(pivot, Some(pfs)).toFlatRows(Totals.Null)

    MarketDataResponse(parameters.update(_.version = Some(version)), data.map(row => MarketDataRow(row.map(_.toString))))
  } catch { case exception => MarketDataResponse(parameters, errors = List(exception.getMessage)) }

  /**
   * get price fixings for the supplied EDM Quota
   */
  def getFixings(quota: EDMQuota) : MarketDataResponse = {

    val mdParams = fixingRequest.update(
      _.addFilters(MarketDataFilter(marketField.name, List("<market>"))),
      _.columns = List(),
      _.rows = names(levelField, periodField)
    )

    marketData(mdParams)
  }

  /**
   * valuation of all Edm quotsa service
   */
  def getAllQuotaValues() : Either[List[Either[EDMQuantity, String], Boolean] = time(getAllQuotaValuesImpl(), "Took %d ms to execute getAllQuotaValues")

  private def getAllQuotaValuesImpl() : EDMQuantity = {
    null
  }

  /**
   * valuation of an Edm quota service
   */
  def getQuotaValue(quotaId : String) : EDMQuantity = time(getQuotaValueImpl(quotaId), "Took %d ms to execute getQuotaValue")

  private def getQuotaValueImpl(quotaId : String) : EDMQuantity = {

    try {
      Log.info("getQuotaValue for %s".format(quotaId))
      val q = quotaById(quotaId)

      Log.info("Found requested quota by id %s { %s }".format(quotaId, q.toString))

      val trades = allTrades()

      val tradeString = StarlingXStream.write(trades)

      val fstream = new FileWriter("/tmp/edmtrades.xml")
      val out = new BufferedWriter(fstream)
      out.write(tradeString)

      out.close()
      fstream.close()


      Log.info("Got %d  edm trades".format(trades.size))


      EDMConversions.toEDMQuantity(
        Quantity(trades.size, UOM.USD)
      )
    }
    catch {
      case e : Exception =>  Log.error("getQuotaValue for %s failed, error: ".format(e.getMessage(), e))
      throw e
    }
  }

  def latestLiborFixings() = marketData(fixingRequest.update(_.filterExchange("LIBOR", "IRS"),
    _.rows = names(levelField, periodField), _.columns = names(marketField)))

  def latestECBFXFixings() = marketData(fixingRequest.update(_.filterExchange("ECB"), _.rows = names(marketField, periodField)))

  def latestLMEFixings() = marketData(fixingRequest.update(_.filterExchange("LME"),
    _.rows = names(marketField, periodField), _.columns = List(levelField.name, "Observation Time")))

  private def fixingRequest = MarketDataRequestParameters(PricingGroup.Metals.name, PriceFixingsHistoryDataType.name, None,
    measures = names(priceField), filters = List(MarketDataFilter("Observation Day", List(Day.today.toString))))

  private def fields(names: List[String]) = names.map(Field(_))
  private def names(fields: FieldDetails*) = fields.map(_.name).toList


  /**
   * tactical ref-data derived maps of ref-data, this could be tidied and made generic etc but it's to be replaced with SRD soon...
   */
  val rmetadminuser = props.ServiceInternalAdminUser()
  val tradeServiceURL = props.EdmTradeServiceUrl()
  val refdataServiceURL = props.TacticalRefDataServiceUrl()


  private def quotaById(id : String) = {

    if (quotasMap.contains(id)) {
      quotasMap(id)
    }
    else {
      Log.info("quota cache miss for quota %s, refeshing cache".format(id))
      quotasMap = quotasSrc()
      quotasMap(id)
    }
  }

  /**
   * tactical ref-data handling...
   */
  lazy val quotasSrc : () => Map[String, QuotaDetail] = () => Map[String, QuotaDetail]() ++ allTrades().flatMap(_.quotas.map(q => (q.detail.identifier, q.detail)))

  // maps of tactical ref-data by id
  lazy val commoditiesSrc : Map[GUID, Metal] = Map[GUID, Metal]() ++ allCommodities.map(e => (e.guid , e))
  lazy val exchangesSrc : Map[GUID, Market] = Map[GUID, Market]() ++ allExchanges.map(e => (e.guid , e))

  // set up a client executor and edm trade proxy
  lazy val tradeResults : () => TradeResults = () => edmGetTradesService.getAll()
  lazy val allTrades : () => List[PhysicalTrade] = () => {
    var tr : TradeResults = TradeResults(cached = false)
    val sw = new Stopwatch()
    while (tr.cached == false){
      println("Waitng for trades " + sw)
      Thread.sleep(5000)
      tr = tradeResults()
    }
    tr.results.map(_.trade.asInstanceOf[PhysicalTrade])
  }


  implicit val clientExecutor : ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val edmGetTradesService : EdmGetTrades = getEdmGetTradesServiceProxy(clientExecutor)
  lazy val tacticalRefdataMetalsService : MetalService = getTacticalRefdataMetalServiceProxy(clientExecutor)
  lazy val tacticalRefdataMarketsService : MarketService = getTacticalRefdataMarketServiceProxy(clientExecutor)
  def allCommodities() = tacticalRefdataMetalsService.getMetals()
  def allExchanges() = tacticalRefdataMarketsService.getMarkets()

  // maps of tactical ref-data by id
  var quotasMap = Map[String, QuotaDetail]() //quotasSrc()

  // debug output received trade quotas
  //Log.debug(quotasMap.values.mkString("\n"))


  /**
   * get proxies for services
   */
  private def getEdmGetTradesServiceProxy(executor : ClientExecutor) : EdmGetTrades =
    new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, executor))

  private def getTacticalRefdataMetalServiceProxy(executor : ClientExecutor) : MetalService =
    new MetalServiceResourceProxy(ProxyFactory.create(classOf[MetalServiceResource], refdataServiceURL, executor))

  private def getTacticalRefdataMarketServiceProxy(executor : ClientExecutor) : MarketService =
    new MarketServiceResourceProxy(ProxyFactory.create(classOf[MarketServiceResource], refdataServiceURL, executor))

  /**
   * generic get proxy utils
   */
  private def getProxy[T : Manifest] : String => AnyRef = getProxy[T](clientExecutor) _

  private def getProxy[T : Manifest](executor : ClientExecutor)(url : String) : AnyRef = {

    val serviceClassName = cname[T]

    val serviceClassResource = serviceClassName + "Resource"
    val serviceClassResourceProxy = serviceClassResource + "Proxy"
    val serviceClazz = Class.forName(serviceClassResource)
    val serviceClazzProxy = Class.forName(serviceClassResourceProxy)
    val constructor = serviceClazzProxy.getConstructor(serviceClazz)
    val proxy = ProxyFactory.create(serviceClazz, url, executor)

    constructor.newInstance(proxy.asInstanceOf[AnyRef]).asInstanceOf[AnyRef]
  }

  def cname[T : Manifest]() = manifest[T].erasure.getName

  def getQuotaValue(quotaId: Int) = null
}

object MarketDataService extends Application {

  def loadAndValueAll() {

    val sw = new Stopwatch()

    new Thread(new Runnable() {
      def run() {  Server.main(Array()) }
    }).start()

    while (Server.server == null){
      Thread.sleep(1000)
    }

    val md = new MarketDataServiceRPC(Server.server.marketDataStore, Server.server.props)

    val trades = md.allTrades()

    val env = Environment(new NullAtomicEnvironment(Day(2010, 1, 1).endOfDay))
    val qv = new QuotaValuer(env, md.exchangesSrc, md.commoditiesSrc)
    println("Exchanges")
    md.allExchanges.foreach(println)
    println("Metals")
    md.allCommodities().foreach(println)

    val valuedTrades = //: List[Either[(Exception, PhysicalTrade), (Quantity, QuotaDetail)]] =
      trades.flatMap(physTrade => {
        physTrade.quotas.map(q => {
          try {
            val value = qv.value(q, physTrade.tradeId)
//            println("Trade " + physTrade.tradeId + ", quota " + q.quotaNumber + ", value = " + value)
            Left((value, q))
          }
          catch {
            case ex : InvalidPricingSpecException => Right((ex, physTrade))
            case ex : NullPointerException => throw ex
            case ex : NoSuchElementException => {
              println("trade id " + physTrade.tradeId + " failed ex : " + ex)
              throw ex
            }
            case ex : Exception => {
              println("trade id " + physTrade.tradeId + " failed ex : " + ex)
              Right((ex, physTrade))
            }
            case _ => Right((null, physTrade))
          }
        })
      })

    val (errors, valuations) = valuedTrades.partition(_ match { case Right(x) => true; case _ => false } )

    println("Worked " + valuations.size + ", failed " + errors.size + ", took " + sw)
  }

  def loadAndValue() {

    //val tradeXml = io.Source.fromFile("/tmp/edmtrades.xml").getLines().mkString("\n")
    val tradeXmlTmp = FileUtils.readFileToString(new File("/tmp/edmtrades.json"))

    val sw = new Stopwatch()
    val trades = tradeXmlTmp.split('\n').map{
      l =>
        PhysicalTrade.fromJson(JSONConversions.parseJSON(l).asInstanceOf[org.codehaus.jettison.json.JSONObject])
    }
    println(sw)

    val env = Environment(new NullAtomicEnvironment(Day(2010, 1, 1).endOfDay))
    val qv = new QuotaValuer(env, md.allExchanges(), md.allCommodities())
    trades.foreach{
      physTrade =>
        println("Trade " + physTrade.tradeId)
        physTrade.quotas.foreach { q =>
            println(qv.value(q))
        }
    }
  }

  //readAndStore()
  //loadAndValueAll()



  def readAndStore() {
    new Thread(new Runnable() {
      def run() {  Server.main(Array()) }
    }).start()

    while (Server.server == null){
      Thread.sleep(1000)
    }

    val md = new MarketDataServiceRPC(Server.server.marketDataStore, Server.server.props)

    val trades = md.allTrades()

    val tradeStrings = trades.map(_.toJson())  //  StarlingXStream.write(trades)

    val fstream = new FileWriter("/tmp/edmtrades.json")
    val out = new BufferedWriter(fstream)
    out.write(tradeStrings.mkString("\n"))

    out.close()
    fstream.close()
  }
}


/**
 * Market data service stub
 *  this service stub impl that overrides the filter chain with a null implementation
 */
class MarketDataServiceResourceStubEx()
    extends MarketDataServiceResourceStub(new MarketDataServiceRPC(Server.server.marketDataStore, Server.server.props), List[ServiceFilter]()) {

  // this is deliberately stubbed out as the exact requirements on permissions and it's implementation for this service is TBD
  override def requireFilters(filterClasses:String*) {}
}
