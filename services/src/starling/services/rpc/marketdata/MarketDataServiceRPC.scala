package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata._
import scala.collection.JavaConversions._

import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.{PriceFixingsHistoryDataType, MarketDataTypes}
import starling.pivot.model.PivotTableModel
import starling.pivot._
import starling.services.Server
import starling.utils.Log

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
import com.trafigura.tradinghub.support.{GUID, ServiceFilter}
import com.trafigura.edm.physicaltradespecs.{PhysicalTradeSpec, QuotaDetail, EDMQuota}
import javax.management.remote.rmi._RMIConnection_Stub
import com.trafigura.edm.trades.PhysicalTrade
import com.trafigura.tradecapture.internal.refinedmetal.{Market, Metal}
import starling.props.{Props, PropsHelper}

import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice.{TranslationsServiceResource, TranslationsServiceResourceProxy}
import com.trafigura.edm.tradeservice.{EdmGetTradesResource, EdmGetTradesResourceProxy, EdmGetTrades}
import com.trafigura.services.security.ComponentTestClientExecutor
import com.trafigura.services.security._
import com.trafigura.timer.Timer._


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

   /*
    * valuation of an Edm quota service
    */
  def getQuotaValue(quotaId : String) : EDMQuantity = time(getQuotaValueImpl(quotaId), "Took %d ms to execute getQuotaValue")

  private def getQuotaValueImpl(quotaId : String) : EDMQuantity = {

    try {
      Log.info("getQuotaValue for %s".format(quotaId))
      val q = quotaById(quotaId)

      val trades = allTrades()
      Log.info("Got %d  edm trades".format(trades.size))

      val commodities = commoditiesSrc()

      Log.info("Got %d  commodities".format(commodities.size))
      val commodityNames = commodities.map(_._2.name).mkString("\n")
      Log.info("Commodities: \n%s".format(commodityNames))

      val exchanges = exchangesSrc()

      Log.info("Got %d  commodities".format(exchanges.size))
      val exchangeNames = exchanges.map(_._2.name).mkString("\n")
      Log.info("Exchanges: \n%s".format(exchanges))

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

  var quotasMap = quotasSrc()
  Log.debug(quotasMap.values.mkString("\n"))

  // maps of tactical ref-data by id
  lazy val quotasSrc : () => Map[String, QuotaDetail] = () => Map[String, QuotaDetail]() ++ allTrades().flatMap(_.quotas.map(q => (q.detail.identifier, q.detail)))
  lazy val commoditiesSrc : () => Map[GUID, Metal] = () => Map[GUID, Metal]() ++ allCommodities().map(e => (e.guid , e))
  lazy val exchangesSrc : () => Map[GUID, Market] = () => Map[GUID, Market]() ++ allExchanges().map(e => (e.guid , e))

  // set up a client executor and edm trade proxy
  lazy val allTrades : () => List[PhysicalTrade] = () => edmGetTradesService.getAll().map(_.asInstanceOf[PhysicalTrade])
  lazy val allCommodities  = () => tacticalRefdataMetalsService.getMetals()
  lazy val allExchanges  = () => tacticalRefdataMarketsService.getMarkets()


  val clientExecutor : ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  val edmGetTradesService : EdmGetTrades = getEdmGetTradesServiceProxy(clientExecutor)
  val tacticalRefdataMetalsService : MetalService = getTacticalRefdataMetalServiceProxy(clientExecutor)
  val tacticalRefdataMarketsService : MarketService = getTacticalRefdataMarketServiceProxy(clientExecutor)

  /**
   * get proxies for services
   */
  private def getEdmGetTradesServiceProxy(executor : ClientExecutor) : EdmGetTrades =
    new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, executor))

  private def getTacticalRefdataMetalServiceProxy(executor : ClientExecutor) : MetalService =
    new MetalServiceResourceProxy(ProxyFactory.create(classOf[MetalServiceResource], refdataServiceURL, executor))

  private def getTacticalRefdataMarketServiceProxy(executor : ClientExecutor) : MarketService =
    new MarketServiceResourceProxy(ProxyFactory.create(classOf[MarketServiceResource], refdataServiceURL, executor))
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
