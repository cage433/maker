package starling.db

import collection.immutable._
import starling.marketdata._
import starling.daterange._
import starling.richdb.RichResultSetRow
import scala.collection.JavaConversions._
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._
import starling.utils.Log


abstract class DBKeyCache[K <: DBKey[K] : Manifest](db: DBTrait[RichResultSetRow]) extends Log {
  protected val table = manifest[K].erasure.getSimpleName
  protected lazy val cache = log.infoWithTime("Populating key cache from: " + table) {
    JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Int, K](
      db.queryWithResult("SELECT * FROM " + table) { create(_) }.toMapWithKeys(_.id)))
  }

  final def apply(id: Int): K = get(id) | findOrUpdate(db.queryWithOneResult(
    "SELECT * FROM %s WHERE id = %d" % (table, id)) { create(_) }.getOrThrow("No %s with id = %d" % (table, id)))

  final def get(id: Int): Option[K] = cache.get(id)
  final def size = cache.size
  final def findOrUpdate(key: K): K = cache.findOrUpdate(_._2.sameValuesAs(key), insert(key) |> (k => (k.id, k)))._2

  protected def create(rs: ResultSetRow): K
  protected def insert(key: K): K

  protected final def insert(table: String, values: Map[String, Any]): Int = db.inTransaction(writer =>
    writer.insertAndReturnKey(table, "id", values).asInstanceOf[Int])
}


class ExtendedKeyCache(db: DBTrait[RichResultSetRow], dataTypes: MarketDataTypes) extends DBKeyCache[MarketDataExtendedKey](db) {
  def idsFor(marketDataType: MarketDataTypeName, mds: List[MarketDataSet],
    observationTimes: Option[Set[ObservationTimeOfDay]] = None, marketDataKeys: Option[Set[MarketDataKey]] = None): List[Int] = {

    cache.filterValues(key => key.marketDataType == dataTypes.fromName(marketDataType) && mds.contains(key.marketDataSet) &&
      observationTimes.fold(times => times.contains(key.time), true) &&
      marketDataKeys.fold(keys => keys.contains(key.marketDataKey), true)
    ).keys.toList.distinct
  }

  protected def create(rs: ResultSetRow) = new KeyHelper(dataTypes).marketDataExtendedKey(rs)
  protected def insert(key: MarketDataExtendedKey) = key.copy(id = insert(table, key.dbMap))
}


class ValueKeyCache(db: DBTrait[RichResultSetRow]) extends DBKeyCache[MarketDataValueKey](db) {
  protected def create(rs: ResultSetRow) = MarketDataValueKey(rs.getObject[Map[String, Any]]("valueKey"), rs.getInt("id"))
  protected def insert(key: MarketDataValueKey) = key.copy(id = insert(table, key.dbMap))
}