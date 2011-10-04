package starling.services.rpc.marketdata

import starling.titan.EDMConversions._
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.quantity.{Quantity, UOM}

import com.trafigura.services._
import marketdata._
import starling.utils.Log
import org.joda.time.LocalDate
import starling.services.rpc.valuation.EnvironmentProvider
import starling.gui.api._
import starling.marketdata._
import starling.daterange.{StoredFixingPeriod, DateRange, Day}
import scalaz.Scalaz._
import starling.db.{SnapshotID, MarketDataStore}


class MarketDataService(marketDataStore: MarketDataStore, environmentProvider: EnvironmentProvider)
  extends MarketDataServiceApi with DocumentedService with Log {

  /** Return all snapshots for a given observation day, or every snapshot if no day is supplied */
  def marketDataSnapshotIDs(observationDay: Option[LocalDate] = None) =
    environmentProvider.snapshotIDs(observationDay.map(Day.fromJodaDate)).map(_.toSerializable)

  def latestSnapshotID() = marketDataStore.latestSnapshot(PricingGroup.Metals).map(_.toSerializable)

  def getSpotFXRate(snapshotId: TitanSnapshotIdentifier, observationDay: TitanSerializableDate,
    fromTSC: TitanSerializableCurrency, toTSC: TitanSerializableCurrency) = {

    notNull("snapshotId" → snapshotId, "observationDay" → observationDay, "from" → fromTSC, "to" → toTSC)

    val (uomFrom, uomTo) = (fromTSC.fromSerializable, toTSC.fromSerializable)

    val rates = spotFXRatesFor(snapshotId, observationDay, SpotFXDataKey(uomFrom), SpotFXDataKey(uomTo))
      .toMapWithKeys(_.uom.denominatorUOM) + (UOM.USD → Quantity.ONE)

    (for (from <- rates.get(uomFrom); to <- rates.get(uomTo)) yield SpotFXRate(snapshotId, observationDay, (from / to).toSerializable))
      .getOrElse(throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s" % (fromTSC, toTSC, snapshotId)))
  }

  def getSpotFXRates(snapshotId: TitanSnapshotIdentifier, observationDay: TitanSerializableDate) = {
    notNull("snapshotId" → snapshotId, "observationDay" → observationDay)

    spotFXRatesFor(snapshotId, observationDay) map { rate => SpotFXRate(snapshotId, observationDay, rate.toSerializable) }
  }

  def getReferenceInterestRate(snapshotId: TitanSnapshotIdentifier, observationDay: TitanSerializableDate,
    source: ReferenceRateSource, maturity: Maturity, currency: TitanSerializableCurrency) = {

    notNull("snapshotId" → snapshotId, "observationDay" → observationDay, "source" → source, "maturity" → maturity, "currency" → currency)

    getReferenceInterestRates(snapshotId, observationDay, observationDay, source, maturity, currency).headOption
      .getOrThrow("No Reference Interest Rate")
  }

  def getReferenceInterestRates(snapshotId: TitanSnapshotIdentifier, from: TitanSerializableDate,
    to: TitanSerializableDate, source: ReferenceRateSource, maturity: Maturity, currency: TitanSerializableCurrency) = {

    notNull("snapshotId" → snapshotId, "from" → from, "to" → to, "source" → source, "maturity" → maturity, "currency" → currency)

    getReferenceInterestRates(snapshotId, from.fromSerializable upto to.fromSerializable,
      PriceFixingsHistoryDataKey(currency.fromSerializable.toString, Some(source.name))).filter(_.maturity == maturity)
  }

  def getReferenceInterestRates(snapshotId: TitanSnapshotIdentifier, observationDay: TitanSerializableDate) = {
    notNull("snapshotId" → snapshotId, "observationDay" → observationDay)

    getReferenceInterestRates(snapshotId, observationDay.fromSerializable)
  }

  private def getReferenceInterestRates(snapshotId: TitanSnapshotIdentifier, observationDates: DateRange, keys: MarketDataKey*) = {
    val fixings = query[PriceFixingsHistoryData](snapshotId, observationDates, PriceFixingsHistoryDataType, keys : _*)

    fixings.collect { case (Some(obsDay), PriceFixingsHistoryDataKey(StringToTitanCurrency(ccy), Some(source)), fixingsForKey) =>
      fixingsForKey.fixings.collect { case ((level, StoredFixingPeriod.Tenor(tenor)), MarketValue.Percentage(rate)) =>
        ReferenceInterestRate(snapshotId, obsDay.toTitan, ReferenceRateSource(source), tenor.toTitan, ccy, rate.toSerializable)
      }
    }.flatten
  }

  private def spotFXRatesFor(snapshotId: TitanSnapshotIdentifier, observationDay: TitanSerializableDate, keys: MarketDataKey*) = {
    val rates = query[SpotFXData](snapshotId, observationDay.fromSerializable, SpotFXDataType, keys : _*)

    rates.collect { case (Some(_), SpotFXDataKey(UOMToTitanCurrency(_)), SpotFXData(rate)) =>
      if (rate.denominatorUOM == UOM.USD) rate.invert else rate
    }
  }

  private def query[MD <: MarketData : Manifest](snapshotId: TitanSnapshotIdentifier, observationDates: DateRange,
    marketDataType: MarketDataType, keys: MarketDataKey*): List[(Option[Day], MarketDataKey, MD)] = {

    marketDataStore.query(identifierFor(snapshotId), marketDataType, some(observationDates.days.map(some(_)).toSet), None, some(keys.toSet))
      .map { case (timedKey, data) => (timedKey.day, timedKey.key, data) }
      .filterCast[(Option[Day], MarketDataKey, MD)]
  }

  private def identifierFor(snapshotId: TitanSnapshotIdentifier) = MarketDataIdentifier(
    MarketDataSelection(Some(PricingGroup.Metals)), SnapshotMarketDataVersion(snapshotIDFor(snapshotId).label))

  private def snapshotIDFor(snapshotId: TitanSnapshotIdentifier): SnapshotID =
    snapshotId.fromTitan(marketDataStore).getOrThrow("No such snapshot: " + snapshotId)
}