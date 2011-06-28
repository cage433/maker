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
import starling.utils.Log
import starling.daterange.{Tenor, Day}
import starling.quantity.{Percentage, Quantity, UOM}

import com.trafigura.services._
import com.trafigura.services.marketdata.{MarketDataServiceApi, ReferenceInterestRate, ReferenceRateSource, Maturity}

abstract class Matcher[A] {
  def matches(value: A): Boolean
}

case class Match[A](pattern: A) extends Matcher[A] {
  def matches(value: A) = pattern == value
}

case class Ignore[A] extends Matcher[A] {
  def matches(value: A) = true
}

class MarketDataService(marketDataStore: MarketDataStore) extends MarketDataServiceApi  {
  implicit def enrichReferenceInterestRate(self: ReferenceInterestRate) = new {
    def matches(observationDate: Matcher[TitanSerializableDate], source: Matcher[ReferenceRateSource], maturity: Matcher[Maturity],
                currency: Matcher[TitanSerializableCurrency], rate: Matcher[TitanSerializablePercentage])

    = observationDate.matches(self.observationDate) && source.matches(self.source) && maturity.matches(self.maturity) &&
      currency.matches(self.currency)
  }

  def getSpotFXRate(from: TitanSerializableCurrency, to: TitanSerializableCurrency, observationDate: TitanSerializableDate)
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
                               currency: TitanSerializableCurrency) = {

    val results = getReferenceInterestRates(observationDate)

    results.find(_.matches(Ignore[TitanSerializableDate], Match(source), Match(maturity), Match(currency), Ignore[TitanSerializablePercentage])).getOrElse(
      throw new IllegalArgumentException(
        "No Reference Interest Rate observed on %s with source: %s, maturity: %s, currency: %s" %
          (observationDate, source, maturity, currency) +
          (", valid sources: %s, maturities: %s, currencies: %s" %
            (results.map(_.source.name).distinct, results.map(_.maturity).distinct, results.map(_.currency.name).distinct))))
  }

  def getReferenceInterestRates(observationDate: TitanSerializableDate): List[ReferenceInterestRate] = {
    val response = getMarketData(fixingRequest.copyObservationDay(observationDate.fromSerializable)
      .copy(rows = List(exchangeField, marketField, periodField)))

    val rates = response.data.map(_.map(_.toString)).collect {
      case List(exchange, TitanSerializableCurrency.Parse(currency), Tenor.Parse(tenor), Percentage.Parse(percentage)) =>
        ReferenceInterestRate(observationDate, ReferenceRateSource(exchange.toString), tenor.toTitan, currency,
          percentage.toSerializable)
    }

    if (rates.isEmpty) throw new IllegalArgumentException("No Reference Interest Rates observed on: " + observationDate.fromSerializable)

    rates
  }

  //  def latestLiborFixings() = getMarketData(fixingRequest.copyExchange("LIBOR", "IRS")
  //    .copy(rows = List(levelField, periodField), columns = List(marketField)))
  //
  //  def latestECBFXFixings() = getMarketData(fixingRequest.copyExchange("ECB").copy(rows = List(marketField, periodField)))
  //
  //  def latestLMEFixings() = getMarketData(fixingRequest.copyExchange("LME").copy(rows = List(marketField, periodField),
  //    columns = List(levelField, FieldDetails("Observation Time"))))

  private def getSpotFXRate(from: UOM, to: UOM, observationDay: Day): Quantity = {
    val rates = getSpotFXRates(observationDay).toMapWithKeys(_.uom)

    (rates.get(from), rates.get(to)) match {
      case (Some(from), Some(to)) => from / to
      case _ => throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s, valid currencies: %s" %
        (from, to, observationDay))
    }
  }

  private def getSpotFXRates(observationDay: Day) = getMarketData(spotFXRequest.copyObservationDay(observationDay))
    .data.collect { case List(currency: UOM, rate: Double) => Quantity(rate, currency) }

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
  def addFilter(name: String, values: String*) = copy(filters = filters + (name → values.toList))

  def pivotFieldsState(pivot: PivotTableDataSource) =
    PivotFieldsState(fields(measures), fields(rows), fields(columns), filters.map {
      case (name, values) => pivot.parseFilter(Field(name), values)
    }.toList)

  private def fields(names: List[FieldDetails]) = names.map(_.field)
}