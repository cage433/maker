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
import starling.daterange.{TimeOfDay, Day}

trait EnvironmentProvider {
  def metalsValuationSnapshots(observationDay : Option[Day]) = snapshots(None).filter(ss =>
    ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) &&  ss.snapshotType == SnapshotType.Valuation && ss.observationDay == observationDay.getOrElse(ss.observationDay)
  )
  def metalsSnapshots(observationDay : Option[Day]) = snapshots(observationDay).filter(ss =>
    ss.marketDataSelection.pricingGroup == Some(PricingGroup.Metals)
  )
  def snapshots(observationDay : Option[Day]) : List[SnapshotID]
  def environment(snapshotID : SnapshotID, observationDay : Day) : Environment
  def environment(marketDataVersion:MarketDataVersion, observationDay:Day):Environment
  def makeValuationSnapshot(version:Int, observationDay : Day):SnapshotID

  def valuationServiceEnvironment(snapshotID : SnapshotID, observationDay : Day, marketDay : Day): Environment = {
    valuationServiceEnvironment(new SnapshotMarketDataVersion(snapshotID.label), observationDay, marketDay)
  }
  def valuationServiceEnvironment(marketDataVersion:MarketDataVersion, observationDay : Day, marketDay : Day): Environment = {
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
    environment(SnapshotMarketDataVersion(snapshotID.label), marketDay)
//    val env = environment(SnapshotMarketDataVersion(snapshotID.label), snapshotID.snapshotDay)
//    val marketDayToUse = List(marketDay.startOfDay, env.marketDay).sortWith(_>_).head
//    env.forwardState(marketDayToUse)
  }

  def environment(marketDataVersion:MarketDataVersion, observationDay:Day):Environment = {
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

class MockEnvironmentProvider() extends EnvironmentProvider {

  private val snapshotsAndData = Map(
    SnapshotID(1, Day(2011, 7, 7).toTimestamp, null, null, None, 0) -> (100.0, 99),
    SnapshotID(2, Day(2011, 7, 7).toTimestamp, null, null, None, 0) ->  (101.0, 98),
    SnapshotID(3, Day(2011, 7, 8).toTimestamp, null, null, None, 0) -> (102.0, 97)
  )
  def snapshots(observationDay : Option[Day]) = metalsValuationSnapshots(observationDay)
  override def metalsValuationSnapshots(observationDay : Option[Day]) : List[SnapshotID] = snapshotsAndData.keySet.toList.filter{
  snapshotID => 
    observationDay match {
      case None => true
      case Some(day) => snapshotID.snapshotDay >= day
    }
  }

  def environment(snapshotID : SnapshotID, marketDay : Day) : Environment = UnitTestingEnvironment(
    marketDay.endOfDay,
    {
      case IndexFixingKey(index, _) => Quantity(snapshotsAndData(snapshotID)._2, index.priceUOM)
      case ForwardPriceKey(market, _, _) => Quantity(snapshotsAndData(snapshotID)._1, market.priceUOM)
      case _: DiscountRateKey => new Quantity(1.0)
    }
  )
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = throw new UnsupportedOperationException

  def environment(marketDataVersion: MarketDataVersion, observationDay: Day) = throw new UnsupportedOperationException
  def makeValuationSnapshot(version: Int, observationDay : Day) = throw new UnsupportedOperationException
}


