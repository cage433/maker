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
  def metalsValuationSnapshots(observationDay : Option[Day]) = snapshots(None).filter(ss =>
    ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) &&  ss.snapshotType == SnapshotType.Valuation && ss.observationDay == observationDay.getOrElse(ss.observationDay)
  )
  def metalsSnapshots(observationDay : Option[Day]) = snapshots(observationDay).filter(ss =>
    ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals)
  )
  def snapshots(observationDay : Option[Day]) : List[SnapshotID]
  def environment(snapshotID : SnapshotID, observationDay : DayAndTime) : Environment
  def environment(marketDataVersion:MarketDataVersion, observationDay:DayAndTime):Environment
  def makeValuationSnapshot(version:Int, observationDay : Day):SnapshotID

  def valuationServiceEnvironment(snapshotID : SnapshotID, observationDay : DayAndTime, marketDay : Day): Environment = {
    valuationServiceEnvironment(new SnapshotMarketDataVersion(snapshotID.label), observationDay, marketDay)
  }
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

  def latestMetalsValuationSnapshot : SnapshotID = metalsValuationSnapshots(None).sortWith{
    case (s1, s2) =>
      if (s1.observationDay == s2.observationDay)
        s1 > s2
      else
        s1.observationDay.get > s2.observationDay.get
   }.head

  /**
   * Used for generating events like 'has value changed'
   */
  def lastValuationSnapshotEnvironment: (SnapshotIDLabel, Environment) = {
    val snapshotID = latestMetalsValuationSnapshot
    val marketDay = snapshotID.observationDay.get
    (snapshotID.label, environment(snapshotID, marketDay.endOfDay))
  }

}


/**
 * Standard environment provider, contains snapshot id name (string) to SnapshotID cache
 *   and a means to get environments by snapshot id name
 */
class DefaultEnvironmentProvider(marketDataStore : MarketDataStore, referenceDataLookup: ReferenceDataLookup) extends EnvironmentProvider with Log {

  private val MetalsSelection = MarketDataSelection(Some(PricingGroup.Metals))

  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)

  def environment(snapshotID: SnapshotID, marketDay : DayAndTime): Environment = {
    environment(SnapshotMarketDataVersion(snapshotID.label), marketDay)
//    val env = environment(SnapshotMarketDataVersion(snapshotID.label), snapshotID.snapshotDay)
//    val marketDayToUse = List(marketDay.startOfDay, env.marketDay).sortWith(_>_).head
//    env.forwardState(marketDayToUse)
  }

  def environment(marketDataVersion:MarketDataVersion, observationDay:DayAndTime):Environment = {
    environmentCache.memoize( (marketDataVersion, observationDay), {
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(MetalsSelection, marketDataVersion))
      new ClosesEnvironmentRule(referenceDataLookup = referenceDataLookup).createEnv(observationDay, reader).environment
    })
  }

  def snapshots(observationDay : Option[Day]) = marketDataStore.snapshots(observationDay)


  def makeValuationSnapshot(version: Int, observationDay : Day) = {
    marketDataStore.snapshot(MarketDataIdentifier(MetalsSelection, SpecificMarketDataVersion(version)), SnapshotType.Valuation, Some(observationDay))
  }
}
