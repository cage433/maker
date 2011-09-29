package starling.services.rpc.valuation

import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.curves.{ClosesEnvironmentRule, Environment}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.utils.cache.CacheFactory
import starling.quantity.Quantity
import starling.curves.DiscountRateKey
import starling.curves.ForwardPriceKey
import starling.curves.UnitTestingAtomicEnvironment
import starling.curves.IndexFixingKey
import com.trafigura.services.{TitanSerializableDate}
import starling.daterange.Day
import starling.utils.Log


trait EnvironmentProvider {
  def getSnapshots() : List[String]
  def environment_(snapshotID : String, marketDay : Option[Day]) : Environment
  def environment(snapshotID : String, marketDay : Option[TitanSerializableDate] = None) : Environment = environment_(snapshotID, marketDay.map{d => Day.fromLocalDate(d.value)})
  def updateSnapshotCache()
  def snapshotNameToID(name : String) : SnapshotID
  def mostRecentSnapshotIdentifierBeforeToday(): Option[String]
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID]
}


/**
 * Standard environment provider, contains snapshot id name (string) to SnapshotID cache
 *   and a means to get environments by snapshot id name
 */
class DefaultEnvironmentProvider(marketDataStore : MarketDataStore) extends EnvironmentProvider with Log {
  private val lock = new Object()
  def getSnapshots() : List[String] = snapshotNameToIDCache.keySet.toList
  def snapshotNameToID(name : String) = snapshotNameToIDCache(name)

  private var snapshotNameToIDCache : Map[String, SnapshotID] = Map[String, SnapshotID]().withDefault(idNameKey => {
    Log.warn("Missing snapshot ID requested, key = '%s', available snapshots in environment provider are \n%s".format(idNameKey, snapshotNameToIDCache.keys.mkString(", ")))
    throw new Exception("Missing snapshot ID requested, key = '%s'".format(idNameKey))
  })

  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)

  def environment_(snapshotIDName: String, marketDay : Option[Day]): Environment = environmentCache.memoize(
    snapshotIDName,
    {snapshotIDName : String => {
      val snapshotID = snapshotNameToIDCache(snapshotIDName)
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshotID.marketDataSelection, snapshotID.version))
      new ClosesEnvironmentRule().createEnv(marketDay.getOrElse(snapshotID.observationDay), reader).environment
    }}
  )

  def updateSnapshotCache() {
    lock.synchronized {
      marketDataStore.snapshots().foreach {
        s : SnapshotID =>
          snapshotNameToIDCache += s.id.toString -> s
      }
    }
  }

  def mostRecentSnapshotIdentifierBeforeToday(): Option[String] = {
    updateSnapshotCache()
    snapshotNameToIDCache.values.toList.filter(_.observationDay < Day.today).sortWith(_ > _).headOption.map(_.id.toString)
  }

  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = {
    updateSnapshotCache()
    snapshotNameToIDCache.values.filter {
      starlingSnapshotID =>
        starlingSnapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) && (observationDay.isEmpty || (starlingSnapshotID.observationDay == observationDay.get))
    }.toList
  }
}

class MockEnvironmentProvider() extends EnvironmentProvider {

  private val snapshotsAndData = Map(
    "Snapshot1" -> (Day(2011, 7, 7), 100.0, 99),
    "Snapshot2" -> (Day(2011, 7, 7), 101.0, 98),
    "Snapshot3" -> (Day(2011, 7, 8), 102.0, 97)
  )
  def getSnapshots() : List[String] = snapshotsAndData.keySet.toList

  def environment_(snapshotID : String, marketDay : Option[Day]) : Environment = Environment(
    new UnitTestingAtomicEnvironment(
      snapshotsAndData(snapshotID)._1.endOfDay,
      {
        case IndexFixingKey(index, _) => Quantity(snapshotsAndData(snapshotID)._3, index.priceUOM)
        case ForwardPriceKey(market, _, _) => Quantity(snapshotsAndData(snapshotID)._2, market.priceUOM)
        case _: DiscountRateKey => new Quantity(1.0)
      }
    )
  )
  def updateSnapshotCache() {}
  def mostRecentSnapshotIdentifierBeforeToday(): Option[String] = Some(getSnapshots().head)
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = throw new UnsupportedOperationException
  def snapshotNameToID(name : String) : SnapshotID = throw new UnsupportedOperationException
}


