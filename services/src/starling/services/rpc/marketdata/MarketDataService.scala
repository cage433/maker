package starling.services.rpc.marketdata

import starling.titan.EDMConversions._
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.quantity.{Quantity, UOM}

import com.trafigura.services._
import com.trafigura.services.marketdata.{MarketDataServiceApi, ReferenceInterestRate, ReferenceRateSource, Maturity}
import starling.utils.Log
import collection.immutable.List
import valuation.TitanSnapshotIdentifier
import org.joda.time.LocalDate
import starling.services.rpc.valuation.EnvironmentProvider
import starling.gui.api._
import starling.db.MarketDataStore
import starling.marketdata._
import starling.daterange.{StoredFixingPeriod, DateRange, Day}
import scalaz.Scalaz._


class MarketDataService(marketDataStore: MarketDataStore, environmentProvider: EnvironmentProvider)
  extends MarketDataServiceApi with DocumentedService with Log {

  /** Return all snapshots for a given observation day, or every snapshot if no day is supplied */
  def marketDataSnapshotIDs(observationDay: Option[LocalDate] = None) =
    environmentProvider.snapshotIDs(observationDay.map(Day.fromJodaDate)).map(_.toSerializable)

  def getSpotFXRate(snapshotId: Option[TitanSnapshotIdentifier], observationDay: Option[TitanSerializableDate],
    fromTSC: TitanSerializableCurrency, toTSC: TitanSerializableCurrency) = {

    notNull("observationDay" → observationDay, "from" → fromTSC, "to" → toTSC)

    val (uomFrom,uomTo) = (fromTSC.fromSerializable, toTSC.fromSerializable)
    val keys: Option[Set[MarketDataKey]] = some(Set(SpotFXDataKey(uomFrom), SpotFXDataKey(uomTo)))

    val rates = spotFXRatesFor(snapshotId, observationDay, keys).toMapWithKeys(_.uom.denominatorUOM) + (UOM.USD → Quantity(1, UOM.SCALAR))

    (rates.get(uomFrom), rates.get(uomTo)) match {
      case (Some(from), Some(to)) => (from / to).toSerializable
      case _ => throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s, valid currencies: [%s]" %
        (fromTSC, toTSC, snapshotId, rates.keySet.mkString(", ")))
    }
  }

  def getSpotFXRates(snapshotId: Option[TitanSnapshotIdentifier], observationDay: Option[TitanSerializableDate]) =
    spotFXRatesFor(snapshotId, observationDay).map(_.toSerializable)

  def getReferenceInterestRate(snapshotId: Option[TitanSnapshotIdentifier], observationDay: Option[TitanSerializableDate],
                               source: ReferenceRateSource, maturity: Maturity,
                               currency: TitanSerializableCurrency) = {

    notNull("source" → source, "maturity" → maturity, "currency" → currency)

    getReferenceInterestRates(snapshotId, observationDay.getOrElse(today), observationDay.getOrElse(today), source, maturity, currency)(0)
  }

  def getReferenceInterestRates(snapshotId: Option[TitanSnapshotIdentifier],
                                from: TitanSerializableDate, to: TitanSerializableDate, source: ReferenceRateSource,
                                maturity: Maturity, currency: TitanSerializableCurrency) = {

    notNull("snapshotId" → snapshotId, "from" → from, "to" → to, "source" → source, "maturity" → maturity, "currency" → currency)

    val keys: Option[Set[MarketDataKey]] =
      some(Set(PriceFixingsHistoryDataKey(currency.fromSerializable.toString, Some(source.name))))

    val results = getReferenceInterestRates(marketDataVersion(snapshotId),
      from.fromSerializable upto to.fromSerializable, keys).filter(_.maturity == maturity)

    if (results.isEmpty) throw new IllegalArgumentException(
      "No Reference Interest Rate observed between %s-%s with source: %s, maturity: %s, currency: %s" %
        (from, to, source, maturity, currency) +
      ("\n\tvalid sources: %s\n\tmaturities: %s\n\tcurrencies: %s" %
        (results.map(_.source.name).distinct, results.map(_.maturity).distinct, results.map(_.currency.name).distinct)))

    results
  }

  def getReferenceInterestRates(snapshotId: Option[TitanSnapshotIdentifier], observationDay: Option[TitanSerializableDate]) = {
    notNull("snapshotId" → snapshotId, "observationDay" → observationDay)

    getReferenceInterestRates(marketDataVersion(snapshotId), observationDay.getOrElse(today).fromSerializable)
  }

  private def getReferenceInterestRates(version: Option[MarketDataVersion], observationDates: DateRange,
    keys: Option[Set[MarketDataKey]] = none[Set[MarketDataKey]]) = {

    val observationDays = some(observationDates.days.map(some(_)).toSet)

    val fixings: List[(Option[Day], PriceFixingsHistoryDataKey, PriceFixingsHistoryData)] =
      marketDataStore.query(identifierFor(version), PriceFixingsHistoryDataType, observationDays, None, keys)
        .map { case (timedKey, data) => (timedKey.day, timedKey.key, data) }
        .filterCast[(Option[Day], PriceFixingsHistoryDataKey, PriceFixingsHistoryData)]

    fixings.collect { case (Some(observationDay), PriceFixingsHistoryDataKey(StringToTitanCurrency(currency), Some(source)), fixingsForKey) =>
      fixingsForKey.fixings.map { case ((level, StoredFixingPeriod.Tenor(tenor)), MarketValue.Percentage(rate)) => {
        ReferenceInterestRate(observationDay.toTitan, ReferenceRateSource(source), tenor.toTitan, currency, rate.toSerializable)
      }}
    }.flatten
  }

  private def spotFXRatesFor(snapshotId: Option[TitanSnapshotIdentifier], observationDay: Option[TitanSerializableDate],
    keys: Option[Set[MarketDataKey]] = none[Set[MarketDataKey]]) = {

    val observationDays = some(Set(some(observationDay.getOrElse(today).fromSerializable)))

    val rates: List[(Option[Day], SpotFXDataKey, SpotFXData)] =
      marketDataStore.query(identifierFor(marketDataVersion(snapshotId)), SpotFXDataType, observationDays, None, keys)
        .map { case (timedKey, data) => (timedKey.day, timedKey.key, data) }
        .filterCast[(Option[Day], SpotFXDataKey, SpotFXData)]

    rates.collect { case (Some(_), SpotFXDataKey(UOMToTitanCurrency(_)), SpotFXData(rate)) =>
      if (rate.denominatorUOM == UOM.USD) rate.invert else rate
    }
  }

  private def today = Day.today.toSerializable
  private def identifierFor(version: Option[MarketDataVersion]) =
    marketDataStore.identifierFor(MarketDataSelection(Some(PricingGroup.Metals)), version)
  private def marketDataVersion(snapshotId: Option[TitanSnapshotIdentifier]) = snapshotId.map(_.toMarketDataVersion).flatOpt
}