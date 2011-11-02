package starling.services.rpc.valuation

import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
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
import starling.gui.api.MarketDataIdentifier._
import starling.gui.api._
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._

trait EnvironmentProvider {
  def metalsValuationSnapshots() : List[SnapshotID]
  def allSnapshots() : List[SnapshotID]
  def environment(snapshotID : SnapshotID, marketDay : Day) : Environment
  def environment(marketDataVersion:MarketDataVersion, observationDay:Day):Environment
  def makeValuationSnapshot(version:Int):SnapshotID

  def environment(marketDataIdentifier : TitanMarketDataIdentifier) : Environment = {
    allSnapshots().find(_.identifier == marketDataIdentifier.snapshotIdentifier.id) match {
      case Some(snapshot) =>
        environment(snapshot, marketDataIdentifier.observationDay)
      case None =>
        throw new SnapshotIDNotFound(marketDataIdentifier.snapshotIdentifier.id)
    }
  }
  def latestMetalsValuationSnapshot : SnapshotID = metalsValuationSnapshots().sortWith(_ > _).head

  /**
   * Used for generating events like 'has value changed'
   */
  def lastValuationSnapshotEnvironment: (SnapshotIDLabel, Environment) = {
    val snapshotID = latestMetalsValuationSnapshot
    val marketDay = Day.today.previousWeekday //This should probably be the observation day used for valuation
                                              //was created but this is not stored. Will fix when this code is used
    (snapshotID.label, environment(snapshotID, marketDay))
  }

}


/**
 * Standard environment provider, contains snapshot id name (string) to SnapshotID cache
 *   and a means to get environments by snapshot id name
 */
class DefaultEnvironmentProvider(marketDataStore : MarketDataStore, referenceDataLookup: ReferenceDataLookup) extends EnvironmentProvider with Log {

  private val MetalsSelection = MarketDataSelection(Some(PricingGroup.Metals))

  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)

  def environment(snapshotID: SnapshotID, marketDay : Day): Environment = {
    val env = environment(SnapshotMarketDataVersion(snapshotID.label), snapshotID.snapshotDay)
      val marketDayToUse = List(marketDay.startOfDay, env.marketDay).sortWith(_>_).head
    env.forwardState(marketDayToUse)
  }

  def environment(marketDataVersion:MarketDataVersion, observationDay:Day):Environment = {
    environmentCache.memoize( (marketDataVersion, observationDay), {
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(MetalsSelection, marketDataVersion))
      new ClosesEnvironmentRule(referenceDataLookup = referenceDataLookup).createEnv(observationDay, reader).environment
    })
  }

  def allSnapshots = marketDataStore.snapshots()

  def metalsValuationSnapshots() = {
    marketDataStore.snapshots().filter(ss =>
      ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) &&  ss.snapshotType == SnapshotType.Valuation
    )
  }


  def makeValuationSnapshot(version: Int) = {
    marketDataStore.snapshot(MarketDataIdentifier(MetalsSelection, SpecificMarketDataVersion(version)), SnapshotType.Valuation)
  }
}

class MockEnvironmentProvider() extends EnvironmentProvider {

  private val snapshotsAndData = Map(
    SnapshotID(1, Day(2011, 7, 7).toTimestamp, null, null, 0) -> (100.0, 99),
    SnapshotID(2, Day(2011, 7, 7).toTimestamp, null, null, 0) ->  (101.0, 98),
    SnapshotID(3, Day(2011, 7, 8).toTimestamp, null, null, 0) -> (102.0, 97)
  )
  def allSnapshots = metalsValuationSnapshots
  def metalsValuationSnapshots() : List[SnapshotID] = snapshotsAndData.keySet.toList

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

  def environment(marketDataVersion: MarketDataVersion, observationDay: Day) = throw new UnsupportedOperationException
  def makeValuationSnapshot(version: Int) = throw new UnsupportedOperationException
}


