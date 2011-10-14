package starling.services.rpc.valuation

import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.utils.cache.CacheFactory
import starling.quantity.Quantity
import com.trafigura.services.{TitanSerializableDate}
import starling.daterange.Day
import starling.utils.Log
import starling.marketdata.ReferenceDataLookup
import starling.curves.NullAtomicEnvironment._
import starling.curves._
import com.trafigura.services.valuation.{TitanMarketDataIdentifier, SnapshotIDNotFound}
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._

trait EnvironmentProvider {
  def snapshots(observationDay : Option[Day] = None) : List[SnapshotID]
  def environment(snapshotID : SnapshotID, marketDay : Day) : Environment

  def environment(marketDataIdentifier : TitanMarketDataIdentifier) : Environment = {
    snapshot(marketDataIdentifier.snapshotIdentifier.id) match {
      case Some(snapshot) =>
        environment(snapshot, marketDataIdentifier.observationDay)
      case None =>
        throw new SnapshotIDNotFound(marketDataIdentifier.snapshotIdentifier.id)
    }
  }
  def mostRecentSnapshotIdentifierBeforeToday : Option[SnapshotID] = {
    snapshots(Some(Day.today)).filter(_.observationDay < Day.today).sortWith(_ > _).headOption
  }

  /**
   * Used for generating events like 'has value changed'
   */
  def recentRepresentativeEnvironment: (String, Environment) = mostRecentSnapshotIdentifierBeforeToday match {
    case Some(snapshotID) => (snapshotID.identifier, environment(snapshotID, Day.today - 1))
    case None => ("No Snapshot found", Environment(NullAtomicEnvironment((Day.today - 1).startOfDay)))
  }

  def snapshot(identifier : String) : Option[SnapshotID] = snapshots(None).find(_.identifier == identifier)
}


/**
 * Standard environment provider, contains snapshot id name (string) to SnapshotID cache
 *   and a means to get environments by snapshot id name
 */
class DefaultEnvironmentProvider(marketDataStore : MarketDataStore, referenceDataLookup: ReferenceDataLookup) extends EnvironmentProvider with Log {

  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)

  def environment(snapshotID: SnapshotID, marketDay : Day): Environment = environmentCache.memoize(
    (snapshotID, marketDay),
    {
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshotID.marketDataSelection, snapshotID.version))
      new ClosesEnvironmentRule(referenceDataLookup = referenceDataLookup).createEnv(marketDay, reader).environment
    }
  )

  def snapshots(observationDay : Option[Day] = None) = observationDay.fold(
    day => snapshotsFor(PricingGroup.Metals).filter(_.observationDay >= day), snapshotsFor(PricingGroup.Metals)).toList.sortWith(_>_)

  private def snapshotsFor(pricingGroup: PricingGroup): List[SnapshotID] =
    marketDataStore.snapshots().filter(_.marketDataSelection.pricingGroup == Some(pricingGroup))
}

class MockEnvironmentProvider() extends EnvironmentProvider {

  private val snapshotsAndData = Map(
    SnapshotID(Day(2011, 7, 7), 1, null, null, 0) -> (100.0, 99),
    SnapshotID(Day(2011, 7, 7), 2, null, null, 0) ->  (101.0, 98),
    SnapshotID(Day(2011, 7, 8), 3, null, null, 0) -> (102.0, 97)
  )
  def snapshots(observationDay : Option[Day]) : List[SnapshotID] = snapshotsAndData.keySet.toList

  def environment(snapshotID : SnapshotID, marketDay : Day) : Environment = Environment(
    new UnitTestingAtomicEnvironment(
      marketDay.endOfDay,
      {
        case IndexFixingKey(index, _) => Quantity(snapshotsAndData(snapshotID)._2, index.priceUOM)
        case ForwardPriceKey(market, _, _) => Quantity(snapshotsAndData(snapshotID)._1, market.priceUOM)
        case _: DiscountRateKey => new Quantity(1.0)
      }
    )
  )
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = throw new UnsupportedOperationException
}


