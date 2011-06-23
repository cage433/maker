package starling.services.rpc.marketdata

import scala.collection.JavaConversions._

import com.trafigura.edm.marketdata._
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.edm.shared.types.{Currency => ECurrency, Date => EDate, DateRange => EDateRange,
                                       Quantity => EDMQuantity, Percentage => EPercentage}
import com.trafigura.tradinghub.support.ServiceFilter

import starling.edm.EDMConversions._
import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.{SpotFXDataType, MarketDataType, PriceFixingsHistoryDataType}
import starling.pivot._
import starling.pivot.model.PivotTableModel
import starling.services.Server
import starling.utils.ImplicitConversions._
import PriceFixingsHistoryDataType._
import SpotFXDataType._
import starling.utils.{Named, StarlingEnum, Log}
import starling.daterange.{Tenor, Day}
import starling.quantity.{Percentage, Quantity, UOM}
import starling.edm.EMaturity


case class EInterestRateType(name: String) extends Named
object EInterestRateType extends StarlingEnum(classOf[EInterestRateType], true) {
  val Unknown = EInterestRateType("Unknown")
}
case class EInterestRatePoint(value: EPercentage, dateRange: EDateRange)

case class EReferenceRateSource(name: String) extends Named
object EReferenceRateSource extends StarlingEnum(classOf[EReferenceRateSource], true) {
  val LIBOR = EReferenceRateSource("LIBOR")
  val JIBAR = EReferenceRateSource("JIBOR")
}


case class EReferenceInterestRate(observationDate: EDate, source: EReferenceRateSource, maturity: EMaturity, currency: ECurrency,
                                  rate: EPercentage) {

  def matches(observationDate: Matcher[EDate], source: Matcher[EReferenceRateSource], maturity: Matcher[EMaturity],
              currency: Matcher[ECurrency], rate: Matcher[EPercentage])

    = observationDate.matches(this.observationDate) && source.matches(this.source) && maturity.matches(this.maturity) &&
      currency.matches(this.currency)
}

abstract class Matcher[A] {
  def matches(value: A): Boolean
}

case class Match[A](pattern: A) extends Matcher[A] {
  def matches(value: A) = pattern == value
}

case class Ignore[A] extends Matcher[A] {
  def matches(value: A) = true
}


/** Generic Market data service, covering all market data */
class MarketDataServiceRPC(marketDataStore: MarketDataStore) extends MarketDataService {
  def marketData(parameters: MarketDataRequestParameters) = throw new Exception("Not available")

  def getMetalsSpotFXRate(from: ECurrency, to: ECurrency, observationDate: EDate): EDMQuantity =
    getMetalsSpotFXRate(from.fromEDM, to.fromEDM, observationDate.fromEDM).toEDM

  def getMetalsSpotFXRates(observationDate: EDate): List[EDMQuantity] = getMetalsSpotFXRates(observationDate.fromEDM).map(_.toEDM)

  def getInterestRates(currency: ECurrency, rateType: EInterestRateType, dateRange: EDateRange): List[EInterestRatePoint] =
    getInterestRates(currency.fromEDM, rateType, dateRange.startDay, dateRange.endDay)

  def getInterestRate(currency: ECurrency, rateType: EInterestRateType, date: EDate): EInterestRatePoint = {
    val rates = getInterestRates(currency.fromEDM, rateType, date.fromEDM, date.fromEDM)

    rates.find(_.dateRange.contains(date)).getOrElse(throw new Exception("No Interest Rate for %s, available dates: %s" %
      (date, rates.map(_.dateRange.fromEDM).mkString(", "))))
  }

  private def getInterestRates(currency: UOM, rateType: EInterestRateType, from: Day, to: Day): List[EInterestRatePoint] = Nil

  def getFixings(quota: EDMQuota): MarketDataResponse = getMarketData(fixingRequest.addFilter(marketField.name, "<market>")
    .copy(columns = List(), rows = List(levelField, periodField))).toEDM

  def getQuotaValue(quotaId : Int) : EDMQuantity = Quantity(1, UOM.USD).toEDM

  def getReferenceInterestRate(observationDate: EDate, source: EReferenceRateSource, maturity: EMaturity, currency: ECurrency) = {
    val results = getReferenceInterestRates(observationDate)

    results.find(_.matches(Ignore[EDate], Match(source), Match(maturity), Match(currency), Ignore[EPercentage])).getOrElse(
      throw new IllegalArgumentException(
        "No Reference Interest Rate observed on %s with source: %s, maturity: %s, currency: %s" %
          (observationDate, source, maturity, currency) +
        (", valid sources: %s, maturities: %s, currencies: %s" %
            results.map(_.source.name), results.map(_.maturity.value), results.map(_.currency.name))))
  }

  def getReferenceInterestRates(observationDate: EDate): List[EReferenceInterestRate] = {
    val response = getMarketData(fixingRequest.copyExchange(EReferenceRateSource.names : _*)
      .copyObservationDay(observationDate.fromEDM).copy(rows = List(exchangeField, marketField, periodField)))

    val rates = response.data.collect {
      case List(EReferenceRateSource.Parse(source), UOM.Parse(uom), tenor: Tenor, percentage: Percentage) =>
        EReferenceInterestRate(observationDate, source, tenor.toEDM, uom.toCurrency, percentage.toEDM)
    }

    if (rates.isEmpty) throw new IllegalArgumentException("No Reference Interest Rates observed on: " + observationDate.fromEDM)

    rates
  }

  def latestLiborFixings() = getMarketData(fixingRequest.copyExchange("LIBOR", "IRS")
    .copy(rows = List(levelField, periodField), columns = List(marketField)))

  def latestECBFXFixings() = getMarketData(fixingRequest.copyExchange("ECB").copy(rows = List(marketField, periodField)))

  def latestLMEFixings() = getMarketData(fixingRequest.copyExchange("LME").copy(rows = List(marketField, periodField),
    columns = List(levelField, FieldDetails("Observation Time"))))

  private def getMetalsSpotFXRate(from: UOM, to: UOM, observationDay: Day): Quantity = {
    val rates = getMetalsSpotFXRates(observationDay).toMapWithKeys(_.uom)

    (rates.get(from), rates.get(to)) match {
      case (Some(from), Some(to)) => from / to
      case _ => throw new IllegalArgumentException("No Metals Spot FX Rate for %s/%s observed on %s, valid currencies: %s" %
        (from, to, observationDay))
    }
  }

  private def getMetalsSpotFXRates(observationDay: Day) = getMarketData(spotFXRequest.copyObservationDay(observationDay))
    .data.collect { case List(currency: UOM, rate: Double) => Quantity(rate, currency) }

  private def getMarketData(parameters: MarketDataRequestParametersCC): MarketDataResponseCC = {
    Log.info("MarketDataServiceRPC called with parameters " + parameters)
    parameters.notNull("Missing parameters")

    val selection = MarketDataSelection(Some(parameters.pricingGroup))
    val version   = parameters.version.getOrElse(marketDataStore.latest(selection))
    val pivot     = marketDataStore.pivot(MarketDataIdentifier(selection, version), parameters.dataType)
    val pfs       = parameters.pivotFieldsState(pivot)
    val data      = PivotTableModel.createPivotTableData(pivot, Some(pfs)).toFlatRows(Totals.Null, trimBlank = true)

    MarketDataResponseCC(parameters.copy(version = Some(version)), data)
  }

  private def fixingRequest = MarketDataRequestParametersCC(PricingGroup.Metals, PriceFixingsHistoryDataType, None,
    measures = List(priceField), filters = Map("Observation Day" → List(Day.today.toString)))

  private def spotFXRequest = MarketDataRequestParametersCC(PricingGroup.Metals, SpotFXDataType, None,
    measures = List(rateField), rows = List(currencyField))
}

case class MarketDataResponseCC(parameters: MarketDataRequestParametersCC, data: List[List[Any]]) {
  def toEDM = MarketDataResponse(parameters.toEDM, data.map(row => MarketDataRow(row.map(_.toString))))
}

case class MarketDataRequestParametersCC(pricingGroup: PricingGroup, dataType: MarketDataType, version: Option[Int] = None,
  measures: List[FieldDetails] = Nil, filters: Map[String, List[String]] = Map(), rows: List[FieldDetails] = Nil,
  columns: List[FieldDetails] = Nil) {

  def copyExchange(exchangeNames: String*) = addFilter(exchangeField.name, exchangeNames : _*)
  def copyObservationDay(day: Day) = addFilter("Observation Day", day.toString)
  def addFilter(name: String, values: String*) = copy(filters = filters + (name → values.toList))

  def pivotFieldsState(pivot: PivotTableDataSource) =
    PivotFieldsState(fields(measures), fields(rows), fields(columns), filters.map {
      case (name, values) => pivot.parseFilter(Field(name), values)
    }.toList)

  def toEDM = MarketDataRequestParameters(pricingGroup.name, dataType.name, version,
    filters.toList.map { case (name, values) => MarketDataFilter(name, values) }, rows.map(_.name), columns.map(_.name))

  private def fields(names: List[FieldDetails]) = names.map(_.field)
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