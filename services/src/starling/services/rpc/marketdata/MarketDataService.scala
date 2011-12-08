package starling.services.rpc.marketdata

import starling.titan.EDMConversions._
import starling.utils.ImplicitConversions._
import com.trafigura.services._
import marketdata._
import starling.utils.Log
import starling.gui.api._
import starling.marketdata._
import scalaz.Scalaz._
import starling.db.{SnapshotID, MarketDataStore}
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import starling.calendar.WeekdayBusinessCalendar
import collection.immutable.Map
import starling.quantity.{Quantity, UOM}
import starling.titan.EDMConversions
import starling.daterange.{Notifier, Location, Day}


class MarketDataService(marketDataStore: MarketDataStore, notifier: Notifier = Notifier.Null)
  extends MarketDataServiceApi with Log {


  /** Return all snapshots for a given observation day, or every snapshot if no day is supplied */
  def marketDataSnapshotIDs(observationDay: Option[Day] = None) : List[TitanSnapshotIdentifier] = marketDataStore.snapshots(observationDay).filter(ss =>
      ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) 
    ).toList.sortWith(_>_).map(_.toSerializable)

  def latestSnapshotID() : Option[TitanSnapshotIdentifier] = marketDataStore.latestSnapshot(PricingGroup.Metals).map(_.toSerializable)

  def getSpotFXRate(marketDataID : TitanMarketDataIdentifier, from: TitanSerializableCurrency, to: TitanSerializableCurrency) = {
    notNull("marketDataID" → marketDataID, "from" → from, "to" → to)

    if (from == to) SpotFXRate(marketDataID, unitRate) else {
      val (uomFrom, uomTo) = (from.fromSerializable, to.fromSerializable)

      val rates = spotFXRatesFor(marketDataID, SpotFXDataKey(uomFrom), SpotFXDataKey(uomTo))(marketDataID.observationDay)

      val rate = rates.toMapWithKeys(_.uom).getOrElse(uomFrom / uomTo,
        throw new IllegalArgumentException("No Spot FX Rate for %s/%s observed on %s" % (from, to, marketDataID)))

      SpotFXRate(marketDataID, rate.toSerializable)
    }
  }

  def getSpotFXRates(marketDataID : TitanMarketDataIdentifier) = {
    notNull("marketDataID" → marketDataID)

    spotFXRatesFor(marketDataID)(marketDataID.observationDay) map { rate => SpotFXRate(marketDataID, rate.toSerializable) }
  }


  def latestSpotFXRates = {
    notifier.notify("latestSpotFXRates")

    val snapshot = latestSnapshotID.getOrElse(
      throw new IllegalArgumentException("No Market Data Snapshots have been made within the last business week"))

    val lastWeek = Day.today.businessDays(weekDay, (-5).to(0))
    val lastWeeksRates = spotFXRatesFor(snapshot, lastWeek)

    lastWeek.reverse.map(observationDay => {
      val mdi = TitanMarketDataIdentifier(snapshot, observationDay, observationDay)

      lastWeeksRates(observationDay).map(q => SpotFXRate(mdi, q.toSerializable))
    }).find(containsAllTitanCurrencies).getOrElse(throw new IllegalArgumentException(
      "No complete set of Spot FX rates within the last business week"))
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
      ForwardRateDataKey(currency.fromSerializable)).filter(rate => rate.maturity == maturity && rate.source == source)
  }

  def getReferenceInterestRates(marketDataID : TitanMarketDataIdentifier) = {
    notNull("marketDataID" → marketDataID)

    getReferenceInterestRates(marketDataID.snapshotIdentifier, marketDataID.observationDay)
  }

  private def getReferenceInterestRates(snapshotId: TitanSnapshotIdentifier, observationDates: Iterable[Day], keys: MarketDataKey*) = {
    val fixings: List[(Option[Day], MarketDataKey, ForwardRateData)] = query[ForwardRateData](snapshotId, observationDates, ForwardRateDataType, keys : _*)

    fixings.collect { case (Some(obsDay), ForwardRateDataKey(UOMToTitanCurrency(ccy)), forwardRates) => forwardRates.rates.mapNested {
      case (source, tenor, rate) =>
        ReferenceInterestRate(obsDay.toTitan, ReferenceRateSource(source.value), tenor.toTitan, ccy, rate.toSerializablePercentage)
    }
    }.flatten.sortBy(_.observationDate.fromSerializable)
  }

  private def spotFXRatesFor(marketDataID: TitanMarketDataIdentifier, keys: MarketDataKey*): MultiMap[Day, Quantity] =
    spotFXRatesFor(marketDataID.snapshotIdentifier, marketDataID.observationDay, keys : _*)

  private def spotFXRatesFor(snapshotID: TitanSnapshotIdentifier, observationDays: Iterable[Day], keys: MarketDataKey*) = {
    val rates = query[SpotFXData](snapshotID, observationDays, SpotFXDataType, keys : _*)

    val allDays = rates.collect { case (Some(observationDay), SpotFXDataKey(UOMToTitanCurrency(_)), SpotFXData(rate)) =>
      (observationDay, if (rate.denominatorUOM == UOM.USD) rate.invert else rate)
    }

    allDays.toMultiMap.mapValues { toUSD =>
      val crosses = (toUSD <|*|> toUSD).map { case (first, second) => first / second }.filterNot(_.isScalar)

      val allCrosses = toUSD ::: toUSD.map(_.invert) ::: crosses

      allCrosses
    }.withDefaultValue(Nil)
  }

  // Only check that all edm currencies are mention once, no need to check for all crosses
  private def containsAllTitanCurrencies(rates: List[SpotFXRate]): Boolean =
    expectedCurrencies.forall(uom => rates.exists(_.rate.uom.contains(uom)))

  private def query[MD <: MarketData : Manifest](snapshotId: TitanSnapshotIdentifier, observationDates: Iterable[Day],
    marketDataType: MarketDataType, keys: MarketDataKey*): List[(Option[Day], MarketDataKey, MD)] = {

    val observationDays: Option[Set[Option[Day]]] = observationDates.ifDefined(_.map(some(_)).toSet)
    val marketDataKeys: Option[Set[MarketDataKey]] = keys.ifDefined(_.toSet)

    marketDataStore.query(identifierFor(snapshotId), marketDataType.name, observationDays, None, marketDataKeys)
      .map { case (timedKey, data) => (timedKey.day, timedKey.key, data) }
      .filterCast[(Option[Day], MarketDataKey, MD)]
  }

  private def identifierFor(snapshotId: TitanSnapshotIdentifier) = MarketDataIdentifier(
    MarketDataSelection(Some(PricingGroup.Metals)), SnapshotMarketDataVersion(snapshotIDFor(snapshotId).label))

  private def snapshotIDFor(snapshotId: TitanSnapshotIdentifier): SnapshotID =
    snapshotId.fromTitan(marketDataStore).getOrThrow("No such snapshot: " + snapshotId)

  private val weekDay = new WeekdayBusinessCalendar(Location.Unknown)
  private val unitRate = TitanSerializableQuantity(1.0, Map.empty)
  private val expectedCurrencies = EDMConversions.starlingCurrencyToEdmCurrency.values.toSet
}
