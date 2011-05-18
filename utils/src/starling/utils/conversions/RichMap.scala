package starling.utils.conversions
import starling.utils.ImplicitConversions._
import collection.SortedMap
import collection.immutable.TreeMap

trait RichMap {
  implicit def enrichMap[K, V](value : Map[K,V]) = new RichMap(value)
  implicit def enrichMultiMap[K, V](value : Map[K, Set[V]]) = new RichMultiMap[K, V](value)

  class RichMap[K,V](map : Map[K,V]) {
    def slice(keys : Any*) : Map[K,V] = if (keys.isEmpty) map else map.filterKeys(key => keys.contains(key))
    def mapValue(key: K, f: V => V): Map[K,V] = map.updated(key, f(map(key)))
    def mapKeys[C](f: K => C): Map[C, V] = map.map(kv => (f(kv._1), kv._2))
    def extendKey[C](f: C => K) = new MapView(map, f)
    def castKeys[C >: K]() = map.asInstanceOf[Map[C, V]]
    def addSome(key: K, value: Option[V]): Map[K,V] = value.map(v => map + key → v).getOrElse(map)
    def addSome(keyValue: (K, Option[V])): Map[K,V] = addSome(keyValue._1, keyValue._2)
    def reverse: Map[V, K] = map.map(_.swap)
    def collectValues[W](collector: PartialFunction[V, W]): Map[K, W] = {
      map.toList.map { case (key, value) => key.optPair(collector.lift(value)) }.somes.toMap
    }
    def filterKeys(keys: Seq[K]) = map.filterKeys(keys.contains)
    def zipMap[W](other: Map[K, W]): Map[K, (V, W)] = {
      val (m, o) = (map.filterKeys(other.keySet), other.filterKeys(map.keySet))
      m.map { case (key, value) => (key, (value, o(key)))}.toMap
    }
    def sortBy(implicit ordering: Ordering[K]): SortedMap[K, V] = TreeMap.empty[K, V](ordering) ++ map
    def sortBy[S](f: K => S)(implicit ordering: Ordering[S]): SortedMap[K, V] = sortBy(ordering.extendTo(f))
  }

  class RichMultiMap[K, V](map : Map[K, Set[V]]) extends RichMap[K, Set[V]](map) {
    def contains(key : K, value : V) : Boolean = map.get(key).map(_.contains(value)).getOrElse(false)
    def contains(pair : (K, V)) : Boolean = contains(pair._1, pair._2)
  }

  class MapView[K, V, C](map: Map[K, V], keyProjection: C => K) {
    def apply(key: C): V = map.apply(keyProjection(key))
    def get(key: C): Option[V] = map.get(keyProjection(key))
    def contains(key: C): Boolean = map.contains(keyProjection(key))
  }
}
