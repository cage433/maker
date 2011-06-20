package starling.services.rpc.marketdata

import scala.collection.JavaConversions._

import com.trafigura.edm.marketdata._
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.edm.shared.types.{Currency => ECurrency, Date => EDate, Quantity => EDMQuantity}
import com.trafigura.tradinghub.support.ServiceFilter

import starling.edm.EDMConversions._
import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.{SpotFXDataType, MarketDataType, PriceFixingsHistoryDataType, MarketDataTypes}
import starling.pivot._
import starling.pivot.model.PivotTableModel
import starling.quantity.{Quantity, UOM}
import starling.services.Server
import starling.utils.Log

import starling.utils.ImplicitConversions._
import PriceFixingsHistoryDataType._
import SpotFXDataType._


case class SpotFXRate(currency: ECurrency, rate: Double)

/** Generic Market data service, covering all market data */
class MarketDataServiceRPC(marketDataStore: MarketDataStore) extends MarketDataService {
  def getMetalsSpotFX(observationDate: EDate) = marketData(spotFXRequest.copyObservationDay(observationDate.fromEDM))
    .collect { case List(UOM.Parse(currency), DoubleParse(rate)) => Quantity(rate, currency).toEDM }

  def marketData(parameters: MarketDataRequestParameters): MarketDataResponse = try {
    Log.info("MarketDataServiceRPC called with parameters " + parameters)
    parameters.notNull("Missing parameters")

    val selection = MarketDataSelection(Some(PricingGroup.fromName(parameters.pricingGroup)))
    val version   = parameters.version.getOrElse(marketDataStore.latest(selection))
    val pivot     = marketDataStore.pivot(MarketDataIdentifier(selection, version), MarketDataTypes.fromName(parameters.dataType))
    val filters   = parameters.filters.map(filter => pivot.parseFilter(Field(filter.name), filter.values))
    val pfs       = PivotFieldsState(fields(parameters.measures), fields(parameters.rows), fields(parameters.columns), filters)
    val data      = PivotTableModel.createPivotTableData(pivot, Some(pfs)).toFlatRows(Totals.Null, trimBlank = true)

    MarketDataResponse(parameters.update(_.version = Some(version)), data.map(row => MarketDataRow(row.map(_.toString))))
  } catch { case exception => MarketDataResponse(parameters, errors = List(exception.getMessage)) }

  def marketData(parameters: MarketDataRequestParametersCC): MarketDataResponse = marketData(parameters.toEDM)

  /**
   * get price fixings for the supplied EDM Quota
   */
  def getFixings(quota: EDMQuota) : MarketDataResponse = {

    val mdParams = fixingRequest.addFilter(marketField.name, "<market>")
      .copy(columns = List(), rows = List(levelField, periodField))

    marketData(mdParams)
  }

  def getQuotaValue(quotaId : Int) : EDMQuantity = Quantity(1, UOM.USD).toEDM

  def latestLiborFixings() = marketData(fixingRequest.copyExchange("LIBOR", "IRE")
    .copy(rows = List(levelField, periodField), columns = List(marketField)))

  def latestECBFXFixings() = marketData(fixingRequest.copyExchange("ECB").copy(rows = List(marketField, periodField)))

  def latestLMEFixings() = marketData(fixingRequest.copyExchange("LME").copy(rows = List(marketField, periodField),
    columns = List(levelField, FieldDetails("Observation Time"))))

  private def fixingRequest = MarketDataRequestParametersCC(PricingGroup.Metals, PriceFixingsHistoryDataType, None,
    measures = List(priceField), filters = Map("Observation Day" → List(Day.today.toString)))

  private def spotFXRequest = MarketDataRequestParametersCC(PricingGroup.Metals, SpotFXDataType, None,
    measures = List(rateField), rows = List(currencyField))

  private def fields(names: List[String]) = names.map(Field(_))
  private def names(fields: FieldDetails*) = fields.map(_.name).toList
}

case class MarketDataRequestParametersCC(pricingGroup: PricingGroup, dataType: MarketDataType, version: Option[Int] = None,
  measures: List[FieldDetails] = Nil, filters: Map[String, List[String]] = Map(), rows: List[FieldDetails] = Nil,
  columns: List[FieldDetails] = Nil) {

  def copyExchange(exchangeNames: String*) = addFilter(exchangeField.name, exchangeNames : _*)
  def copyObservationDay(day: Day) = addFilter("Observation Day", day.toString)
  def addFilter(name: String, values: String*) = copy(filters = filters + (name → values.toList))

  def toEDM = MarketDataRequestParameters(pricingGroup.name, dataType.name, version,
    filters.toList.map { case (name, values) => MarketDataFilter(name, values) }, rows.map(_.name), columns.map(_.name))
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