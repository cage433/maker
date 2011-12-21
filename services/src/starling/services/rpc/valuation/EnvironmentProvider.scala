package starling.services.rpc.valuation

import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import com.trafigura.edm.trademgmt.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.utils.cache.CacheFactory
import starling.quantity.Quantity
import com.trafigura.services.{TitanSerializableDate}
import starling.utils.Log
import starling.marketdata.ReferenceDataLookup
import starling.curves.NullAtomicEnvironment._
import starling.curves._
import com.trafigura.services.valuation.{TitanMarketDataIdentifier, SnapshotIDNotFound}
import starling.gui.api.MarketDataIdentifier._
import starling.gui.api._
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import starling.daterange.{DayAndTime, TimeOfDay, Day}

trait EnvironmentProvider {
  def metalsSnapshots(observationDay : Option[Day]) = snapshots(observationDay).filter(ss =>
    ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals)
  )
  def snapshots(observationDay : Option[Day]) : List[SnapshotID]
  def environment(marketDataVersion:MarketDataVersion, observationDay:DayAndTime):Environment
  def makeValuationSnapshot(version:Int):SnapshotID

  def valuationServiceEnvironment(marketDataVersion:MarketDataVersion, observationDay : DayAndTime, marketDay : Day): Environment = {
    environment(marketDataVersion, observationDay).undiscounted.forwardState(marketDay.endOfDay)
  }

  def snapshotIDFromIdentifier(identifier : String) : SnapshotID = {
    snapshots(None).find(_.identifier == identifier) match {
      case Some(snapshotID) => snapshotID
      case None =>
        throw new SnapshotIDNotFound(identifier)
    }
  }

  def latestMetalsSnapshot : SnapshotID = metalsSnapshots(None).sortWith(_ > _).head

  /**
   * Used for generating events like 'has value changed'
   */
  def lastValuationSnapshotEnvironment: (SnapshotIDLabel, Environment) = {
    val snapshotID = latestMetalsSnapshot
    val observationDay = Day.today.endOfDay
    (snapshotID.label, environment(SnapshotMarketDataVersion(snapshotID.label), observationDay))
  }

}


/**
 * Standard environment provider, contains snapshot id name (string) to SnapshotID cache
 * and a means to get environments by snapshot id name
 */
class DefaultEnvironmentProvider(marketDataStore : MarketDataStore, referenceDataLookup: ReferenceDataLookup) extends EnvironmentProvider with Log {

  private val MetalsSelection = MarketDataSelection(Some(PricingGroup.Metals))

  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)

  def environment(snapshotID: SnapshotID, observationDay : DayAndTime): Environment = {
    environment(SnapshotMarketDataVersion(snapshotID.label), observationDay)
  }

  def environment(marketDataVersion:MarketDataVersion, observationDay:DayAndTime):Environment = {
    environmentCache.memoize( (marketDataVersion, observationDay), {
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(MetalsSelection, marketDataVersion))
      new MostRecentClosesEnvironmentRule(referenceDataLookup = referenceDataLookup).createEnv(observationDay, reader).environment
    })
  }

  def snapshots(observationDay : Option[Day]) = marketDataStore.snapshots(observationDay)


  def makeValuationSnapshot(version: Int) = {
    marketDataStore.snapshot(MarketDataIdentifier(MetalsSelection, SpecificMarketDataVersion(version)), SnapshotType.Valuation)
  }
}


