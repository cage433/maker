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
import com.trafigura.services.valuation.TitanMarketDataIdentifier


class MarketDataService(marketDataStore: MarketDataStore)
  extends MarketDataServiceApi with Log {

  /** Return all snapshots for a given observation day, or every snapshot if no day is supplied */
  def marketDataSnapshotIDs(observationDay: Option[Day] = None) : List[TitanSnapshotIdentifier] = marketDataStore.snapshots.filter(ss =>
      ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) 
    ).toList.sortWith(_>_).map(_.toSerializable)

  def latestSnapshotID() : Option[TitanSnapshotIdentifier] = marketDataStore.latestSnapshot(PricingGroup.Metals).map(_.toSerializable)

  def getSpotFXRate(marketDataID : TitanMarketDataIdentifier, from: TitanSerializableCurrency, to: TitanSerializableCurrency) = {
    notNull("marketDataID" → marketDataID, "from" → from, "to" → to)

    if (from == to) SpotFXRate(marketDataID, unitRate) else {
      val (uomFrom, uomTo) = (from.fromSerializable, to.fromSerializable)

      val rates = spotFXRatesFor(marketDataID, SpotFXDataKey(uomFrom), SpotFXDataKey(uomTo)).toMapWithKeys(_.uom)

      val rate = rates.getOrElse(uomFrom / uomTo,
        throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s" % (from, to, marketDataID)))

      SpotFXRate(marketDataID, rate.toSerializable)
    }
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
    }.flatten.sortBy(_.observationDate.fromSerializable)
  }

  private def spotFXRatesFor(marketDataID : TitanMarketDataIdentifier, keys: MarketDataKey*) = {
    val rates = query[SpotFXData](marketDataID.snapshotIdentifier, marketDataID.observationDay, SpotFXDataType, keys : _*)

    val toUSD = rates.collect { case (Some(_), SpotFXDataKey(UOMToTitanCurrency(_)), SpotFXData(rate)) =>
      if (rate.denominatorUOM == UOM.USD) rate.invert else rate
    }

    val crosses = (toUSD <|*|> toUSD).map { case (first, second) => first / second }.filterNot(_.isScalar)

    val allCrosses = toUSD ::: toUSD.map(_.invert) ::: crosses

    allCrosses
  }

  private def query[MD <: MarketData : Manifest](snapshotId: TitanSnapshotIdentifier, observationDates: DateRange,
    marketDataType: MarketDataType, keys: MarketDataKey*): List[(Option[Day], MarketDataKey, MD)] = {

    val observationDays: Option[Set[Option[Day]]] = observationDates.days.ifDefined(_.map(some(_)).toSet)
    val marketDataKeys: Option[Set[MarketDataKey]] = keys.ifDefined(_.toSet)

    marketDataStore.query(identifierFor(snapshotId), marketDataType.name, observationDays, None, marketDataKeys)
      .map { case (timedKey, data) => (timedKey.day, timedKey.key, data) }
      .filterCast[(Option[Day], MarketDataKey, MD)]
  }

  private def identifierFor(snapshotId: TitanSnapshotIdentifier) = MarketDataIdentifier(
    MarketDataSelection(Some(PricingGroup.Metals)), SnapshotMarketDataVersion(snapshotIDFor(snapshotId).label))

  private def snapshotIDFor(snapshotId: TitanSnapshotIdentifier): SnapshotID =
    snapshotId.fromTitan(marketDataStore).getOrThrow("No such snapshot: " + snapshotId)

  private val unitRate = TitanSerializableQuantity(1.0, Map.empty)
}
