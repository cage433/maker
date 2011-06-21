package starling.services.rpc.marketdata

import scala.collection.JavaConversions._

import com.trafigura.edm.marketdata._
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.edm.shared.types.{Currency => ECurrency, Date => EDate, DateRange => EDateRange,
                                       Quantity => EDMQuantity, Percentage => EPercentage}
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
import starling.utils.ImplicitConversions._
import PriceFixingsHistoryDataType._
import SpotFXDataType._
import starling.utils.{Named, StarlingEnum, Log}

case class EInterestRateType(name: String) extends Named
object EInterestRateType extends StarlingEnum(classOf[EInterestRateType], true) {
  val Unknown = EInterestRateType("Unknown")
}
case class EInterestRatePoint(value: EPercentage, dateRange: EDateRange)


/** Generic Market data service, covering all market data */
class MarketDataServiceRPC(marketDataStore: MarketDataStore) extends MarketDataService {
  def marketData(parameters: MarketDataRequestParameters) = throw new Exception("Not available")

  def getMetalsSpotFXRate(from: ECurrency, to: ECurrency, observationDate: EDate): EDMQuantity = {
    val rates = getMetalsSpotFXRates(observationDate.fromEDM).toMapWithKeys(_.uom)

    (rates(from.fromEDM) / rates(to.fromEDM)).toEDM
  }

  def getMetalsSpotFXRates(observationDate: EDate): List[EDMQuantity] = getMetalsSpotFXRates(observationDate.fromEDM).map(_.toEDM)

  def getInterestRates(currency: ECurrency, rateType: EInterestRateType, dateRange: EDateRange): List[EInterestRatePoint] =
    getInterestRates(currency.fromEDM, rateType, dateRange.startDay, dateRange.endDay)

  def getInterestRate(currency: ECurrency, rateType: EInterestRateType, date: EDate): EInterestRatePoint = {
    val rates = getInterestRates(currency.fromEDM, rateType, date.fromEDM, date.fromEDM)

    rates.find(_.dateRange.contains(date)).getOrElse(throw new Exception("No rate for %s, available dates: %s" %
      (date, rates.map(_.dateRange.fromEDM).mkString(", "))))
  }

  private def getInterestRates(currency: UOM, rateType: EInterestRateType, from: Day, to: Day): List[EInterestRatePoint] = Nil

  def getFixings(quota: EDMQuota): MarketDataResponse = getMarketData(fixingRequest.addFilter(marketField.name, "<market>")
    .copy(columns = List(), rows = List(levelField, periodField)).toEDM)

  def getQuotaValue(quotaId : Int) : EDMQuantity = Quantity(1, UOM.USD).toEDM

  def latestLiborFixings() = getMarketData(fixingRequest.copyExchange("LIBOR", "IRE")
    .copy(rows = List(levelField, periodField), columns = List(marketField)).toEDM)

  def latestECBFXFixings() = getMarketData(fixingRequest.copyExchange("ECB").copy(rows = List(marketField, periodField)).toEDM)

  def latestLMEFixings() = getMarketData(fixingRequest.copyExchange("LME").copy(rows = List(marketField, periodField),
    columns = List(levelField, FieldDetails("Observation Time"))).toEDM)

  implicit def enrichMarketDataResponse(response: MarketDataResponse) = new {
    def map[A](f: MarketDataRow => A) = response.rows.map(f)
    def collect[A](f: PartialFunction[List[String], A]) = response.rows.flatMap(row => f.lift(row.data))
  }

  private def getMetalsSpotFXRates(obseravtionDay: Day) = getMarketData(spotFXRequest.copyObservationDay(obseravtionDay).toEDM)
    .collect { case List(UOM.Parse(currency), DoubleParse(rate)) => Quantity(rate, currency) }

  private def getMarketData(parameters: MarketDataRequestParameters): MarketDataResponse = try {
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