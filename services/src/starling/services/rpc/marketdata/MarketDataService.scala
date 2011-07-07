package starling.services.rpc.marketdata

import starling.edm.EDMConversions._
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.{SpotFXDataType, MarketDataType, PriceFixingsHistoryDataType}
import starling.pivot._
import starling.db.MarketDataStore
import starling.pivot.model.PivotTableModel
import starling.utils.ImplicitConversions._
import PriceFixingsHistoryDataType._
import SpotFXDataType._
import starling.quantity.{Percentage, Quantity, UOM}

import com.trafigura.services._
import com.trafigura.services.marketdata.{MarketDataServiceApi, ReferenceInterestRate, ReferenceRateSource, Maturity}
import starling.utils.Log
import collection.immutable.List
import starling.daterange.{DateRange, Tenor, Day}
import starling.services.Server


class MarketDataServiceStub extends MarketDataService(Server.server.marketDataStore)

class MarketDataService(marketDataStore: MarketDataStore) extends MarketDataServiceApi  {
  type Matcher[T] = T => Boolean

  implicit def enrichReferenceInterestRate(self: ReferenceInterestRate) = new {
    def matches(observationDate: Matcher[TitanSerializableDate], source: Matcher[ReferenceRateSource], maturity: Matcher[Maturity],
                currency: Matcher[TitanSerializableCurrency], rate: Matcher[TitanSerializablePercentage])

    = observationDate(self.observationDate) && source(self.source) && maturity(self.maturity) && currency(self.currency)
  }

  def getSpotFXRate(observationDate: TitanSerializableDate, from: TitanSerializableCurrency, to: TitanSerializableCurrency)
    : TitanSerializableQuantity =
    getSpotFXRate(from.fromSerializable, to.fromSerializable, observationDate.fromSerializable).toSerializable

  def getSpotFXRates(observationDate: TitanSerializableDate): List[TitanSerializableQuantity] =
    getSpotFXRates(observationDate.fromSerializable).map(_.toSerializable)

  //  def getInterestRates(currency: CurrencyE, rateType: EInterestRateType, dateRange: DateERange): List[EInterestRatePoint] =
  //    getInterestRates(currency.fromEDM, rateType, dateRange.startDay, dateRange.endDay)
  //
  //  def getInterestRate(currency: CurrencyE, rateType: EInterestRateType, date: DateE): EInterestRatePoint = {
  //    val rates = getInterestRates(currency.fromEDM, rateType, date.fromEDM, date.fromEDM)
  //
  //    rates.find(_.dateRange.contains(date)).getOrElse(throw new Exception("No Interest Rate for %s, available SerializableDate: %s" %
  //      (date, rates.map(_.dateRange.fromEDM).mkString(", "))))
  //  }

  //  private def getInterestRates(currency: UOM, rateType: EInterestRateType, from: Day, to: Day): List[EInterestRatePoint] = Nil

  //  def getFixings(quota: EDMQuota): MarketDataResponse = getMarketData(fixingRequest.addFilter(marketField.name, "<market>")
  //    .copy(columns = List(), rows = List(levelField, periodField))).toEDM

  def getReferenceInterestRate(observationDate: TitanSerializableDate, source: ReferenceRateSource, maturity: Maturity,
                               currency: TitanSerializableCurrency): ReferenceInterestRate = {

    getReferenceInterestRates(observationDate.toDateRange, source, maturity, currency)(0)
  }

  def getReferenceInterestRates(observationDates: TitanSerializableDateRange, source: ReferenceRateSource, maturity: Maturity,
                                currency: TitanSerializableCurrency) = {

    val results = getReferenceInterestRates(observationDates).filter(_.matches(
      ignore[TitanSerializableDate], eql(source), eql(maturity), eql(currency), ignore[TitanSerializablePercentage]))

    if (results.isEmpty) throw new IllegalArgumentException(
      "No Reference Interest Rate observed between %s with source: %s, maturity: %s, currency: %s" %
        (observationDates, source, maturity, currency) +
      ("\n\tvalid sources: %s\n\tmaturities: %s\n\tcurrencies: %s" %
        (results.map(_.source.name).distinct, results.map(_.maturity).distinct, results.map(_.currency.name).distinct)))

    results
  }

  def getReferenceInterestRates(observationDate: TitanSerializableDate) = getReferenceInterestRates(observationDate.toDateRange)

  private def getReferenceInterestRates(observationDates: TitanSerializableDateRange): List[ReferenceInterestRate] = {
    val response = getMarketData(fixingRequest.copyObservationDays(observationDates.fromSerializable)
      .copy(rows = List(new FieldDetails("Observation Day"), exchangeField, marketField, periodField)))

    response.data.map(_.map(_.toString)).collect {
      case List(Day(date), exchange, TitanSerializableCurrency.Parse(currency), Tenor.Parse(tenor), Percentage.Parse(percentage)) =>
        ReferenceInterestRate(date.toTitan, ReferenceRateSource(exchange.toString), tenor.toTitan, currency, percentage.toSerializable)
    }
  }

  private def ignore[T] = (input: T) => true
  private def eql[T](equalTo: T) = (input: T) => equalTo == input

  private def getSpotFXRate(from: UOM, to: UOM, observationDay: Day): Quantity = {
    val rates = getSpotFXRates(observationDay).toMapWithKeys(_.uom.denominatorUOM) + (UOM.USD → Quantity(1, UOM.SCALAR))

    (rates.get(from), rates.get(to)) match {
      case (Some(from), Some(to)) => from / to
      case _ => throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s, valid currencies: [%s]" %
        (from, to, observationDay, rates.keySet.mkString(", ")))
    }
  }

  private def getSpotFXRates(observationDay: Day) = getMarketData(spotFXRequest.copyObservationDay(observationDay))
    .data.map(_.map(_.toString)).collect {
      case List(_, Quantity.Parse(rate)) => if (rate.denominatorUOM == UOM.USD) rate.invert else rate
    }

  private def getMarketData(parameters: MarketDataRequestParameters) = {
    Log.info("MarketDataServiceRPC called with parameters " + parameters)
    parameters.notNull("Missing parameters")

    val selection = MarketDataSelection(Some(parameters.pricingGroup))
    val version = parameters.version.getOrElse(marketDataStore.latest(selection))
    val pivot = marketDataStore.pivot(MarketDataIdentifier(selection, version), parameters.dataType)
    val pfs = parameters.pivotFieldsState(pivot)
    val data = PivotTableModel.createPivotTableData(pivot, Some(pfs)).toFlatRows(Totals.Null, trimBlank = true)

    MarketDataResponse(parameters.copy(version = Some(version)), data)
  }

  private val fixingRequest = MarketDataRequestParameters(PricingGroup.Metals, PriceFixingsHistoryDataType, None,
    measures = List(priceField), filters = Map("Observation Day" → List(Day.today.toString)))

  private val spotFXRequest = MarketDataRequestParameters(PricingGroup.Metals, SpotFXDataType, None,
    measures = List(rateField), rows = List(currencyField))
}

case class MarketDataResponse(parameters: MarketDataRequestParameters, data: List[List[Any]])

case class MarketDataRequestParameters(pricingGroup: PricingGroup, dataType: MarketDataType, version: Option[Int] = None,
                                       measures: List[FieldDetails] = Nil, filters: Map[String, List[String]] = Map(),
                                       rows: List[FieldDetails] = Nil, columns: List[FieldDetails] = Nil) {

  def copyExchange(exchangeNames: String*) = addFilter(exchangeField.name, exchangeNames: _*)
  def copyObservationDay(day: Day) = addFilter("Observation Day", day.toString)
  def copyObservationDays(dateRange: DateRange) = addFilter("Observation Day", dateRange.days.map(_.toString) : _*)
  def addFilter(name: String, values: String*) = copy(filters = filters + (name → values.toList))

  def pivotFieldsState(pivot: PivotTableDataSource) =
    PivotFieldsState(fields(measures), fields(rows), fields(columns), filters.map {
      case (name, values) => pivot.parseFilter(Field(name), values)
    }.toList)

  private def fields(names: List[FieldDetails]) = names.map(_.field)
}