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
import valuation.TitanMarketDataIdentifier


class MarketDataService(marketDataStore: MarketDataStore, environmentProvider: EnvironmentProvider)
  extends MarketDataServiceApi with DocumentedService with Log {

  /** Return all snapshots for a given observation day, or every snapshot if no day is supplied */
  def marketDataSnapshotIDs(observationDay: Option[Day] = None) =
    environmentProvider.snapshots(observationDay).map(_.toSerializable)

  def latestSnapshotID() = marketDataStore.latestSnapshot(PricingGroup.Metals).map(_.toSerializable)

  def getSpotFXRate(marketDataID : TitanMarketDataIdentifier,
    fromTSC: TitanSerializableCurrency, toTSC: TitanSerializableCurrency) = {

    notNull("marketDataID" → marketDataID, "from" → fromTSC, "to" → toTSC)

    val (uomFrom, uomTo) = (fromTSC.fromSerializable, toTSC.fromSerializable)

    val rates = spotFXRatesFor(marketDataID, SpotFXDataKey(uomFrom), SpotFXDataKey(uomTo))
      .toMapWithKeys(_.uom.denominatorUOM) + (UOM.USD → Quantity.ONE)

    (for (from <- rates.get(uomFrom); to <- rates.get(uomTo)) yield SpotFXRate(marketDataID, (from / to).toSerializable))
      .getOrElse(throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s" % (fromTSC, toTSC, marketDataID)))
  }

  def getSpotFXRates(marketDataID : TitanMarketDataIdentifier) = {
    notNull("marketDataID" → marketDataID)

    spotFXRatesFor(marketDataID) map { rate => SpotFXRate(marketDataID, rate.toSerializable) }
  }

  def getReferenceInterestRate(marketDataID : TitanMarketDataIdentifier,
    source: ReferenceRateSource, maturity: Maturity, currency: TitanSerializableCurrency) = {

    notNull("marketDataID" → marketDataID, "source" → source, "maturity" → maturity, "currency" → currency)

    getReferenceInterestRates(marketDataID.snapshotIdentifier, marketDataID.observationDay, marketDataID.observationDay, source, maturity, currency).headOption
      .getOrThrow("No Reference Interest Rate")
  }

  def getReferenceInterestRates(snapshotId: TitanSnapshotIdentifier, from: Day,
    to: Day, source: ReferenceRateSource, maturity: Maturity, currency: TitanSerializableCurrency): List[ReferenceInterestRate] = {

    notNull("snapshotId" → snapshotId, "from" → from, "to" → to, "source" → source, "maturity" → maturity, "currency" → currency)

    getReferenceInterestRates(snapshotId, from upto to,
      PriceFixingsHistoryDataKey(currency.fromSerializable.toString, Some(source.name))).filter(_.maturity == maturity)
  }

  def getReferenceInterestRates(marketDataID : TitanMarketDataIdentifier) = {
    notNull("marketDataID" → marketDataID)

    getReferenceInterestRates(marketDataID.snapshotIdentifier, marketDataID.observationDay)
  }

  private def getReferenceInterestRates(snapshotId: TitanSnapshotIdentifier, observationDates: DateRange, keys: MarketDataKey*) = {
    val fixings = query[PriceFixingsHistoryData](snapshotId, observationDates, PriceFixingsHistoryDataType, keys : _*)

    fixings.collect { case (Some(obsDay), PriceFixingsHistoryDataKey(StringToTitanCurrency(ccy), Some(source)), fixingsForKey) =>
      fixingsForKey.fixings.collect { case ((level, StoredFixingPeriod.Tenor(tenor)), MarketValue.Percentage(rate)) =>
        ReferenceInterestRate(obsDay.toTitan, ReferenceRateSource(source), tenor.toTitan, ccy, rate.toSerializable)
      }
    }.flatten
  }

  private def spotFXRatesFor(marketDataID : TitanMarketDataIdentifier, keys: MarketDataKey*) = {
    val rates = query[SpotFXData](marketDataID.snapshotIdentifier, marketDataID.observationDay, SpotFXDataType, keys : _*)

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
