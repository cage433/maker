package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata._
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.tradinghub.support.ServiceFilter
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


/** Generic Market data service, covering all market data */
class MarketDataServiceRPC(marketDataStore: MarketDataStore) extends MarketDataService {
  implicit def enrichMarketDataRequestParameters(parameters: MarketDataRequestParameters) = new {
    def filterExchange(exchangeNames: String*) = addFilters(MarketDataFilter(exchangeField.name, exchangeNames.toList))
    def addFilters(filter: MarketDataFilter*) = parameters.filters = parameters.filters ::: filter.toList
  }

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

  def latestLiborFixings() = marketData(fixingRequest.update(_.filterExchange("LIBOR", "IRS"),
    _.rows = names(levelField, periodField), _.columns = names(marketField)))

  def latestECBFXFixings() = marketData(fixingRequest.update(_.filterExchange("ECB"), _.rows = names(marketField, periodField)))

  def latestLMEFixings() = marketData(fixingRequest.update(_.filterExchange("LME"),
    _.rows = names(marketField, periodField), _.columns = List(levelField.name, "Observation Time")))

  private def fixingRequest = MarketDataRequestParameters(PricingGroup.Metals.name, PriceFixingsHistoryDataType.name, None,
    measures = names(priceField), filters = List(MarketDataFilter("Observation Day", List(Day.today.toString))))

  private def fields(names: List[String]) = names.map(Field(_))
  private def names(fields: FieldDetails*) = fields.map(_.name).toList
}

/**
 * Market data service stub
 *  this service stub impl that overrides the filter chain with a null implementation
 */
class MarketDataServiceResourceStubEx()
    extends MarketDataServiceResourceStub(new MarketDataServiceRPC(Server.server.marketDataStore), List[ServiceFilter]()) {

  // this is deliberately stubbed out as the exact requirements on permissions and it's implementation for this service is TBD
  override def requireFilters(filterClasses:String*) {}
}