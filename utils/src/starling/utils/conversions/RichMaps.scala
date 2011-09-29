package starling.utils.conversions
import starling.utils.ImplicitConversions._
import collection.SortedMap
import starling.utils.Pattern.Extractor
import collection.mutable.{Map => MMap}
import scalaz.Scalaz._
import collection.immutable.{Map, TreeMap}

trait RichMaps {
  type MultiMap[K, V] = Map[K, List[V]]

  implicit def enrichMap[K, V](value : Map[K,V]) = new RichMap(value)
  implicit def enrichMultiMap[K, V](value : MultiMap[K, V]) = new RichMultiMap[K, V](value)
  implicit def enrichNestedMap[K1, K2, V](value: Map[K1, Map[K2, V]]) = new RichMap[K1, Map[K2, V]](value) {
    def flipNesting = value.toList.flatMap { case (k1, k2vs) => k2vs.map { case (k2, v) => (k2, (k1, v)) } }
      .groupInto(_.head, _.tail).mapValues(_.toMap)
  }
  implicit def enrichMutableMap[K, V](value: MMap[K, V]) = new RichMutableMap(value)

  def MultiMap[K, V](entries: (K, List[V])*): MultiMap[K, V] = entries.toMap
}

class RichMap[K,V](map : Map[K,V]) {
  def get(key: Option[K]) = key.map(map.get(_)).flatOpt
  def getOrThrow(key: K, msg: String) = map.getOrElse(key, throw new Exception(msg))
  def either(key: K): Either[K, V] = map.get(key).either(key, identity)
  def getOrUpdate(k: K, f: (V) => V) = map.get(k).fold(v => map.updated(k, f(v)), map)
  def slice(keys : Any*) : Map[K,V] = if (keys.isEmpty) map else map.filterKeys(key => keys.contains(key))
  def mapValue(key: K, f: V => V): Map[K,V] = map.updated(key, f(map(key)))
  def mapKeys[C](f: K => C): Map[C, V] = map.map(kv => (f(kv._1), kv._2))
  def composeKeys[C](f: C => K) = new MapView(map, f)
  def castKeys[C >: K]() = map.asInstanceOf[Map[C, V]]
  def addSome(key: K, value: Option[V]): Map[K,V] = value.map(v => map + key → v).getOrElse(map)
  def addSome(keyValue: (K, Option[V])): Map[K,V] = addSome(keyValue._1, keyValue._2)
  def reverse: Map[V, K] = map.map(_.swap)
  def collectKeys[C](pf: PartialFunction[K, C]): Map[C, V] = map.collect(pf *** identity[V] _)
  def collectValues[W](pf: PartialFunction[V, W]): Map[K, W] = map.collect(identity[K] _ *** pf)
  def collectValuesO[W](f: V => Option[W]): Map[K, W] = map.mapValues(f).collectValues { case value if value.isDefined => value.get }
  def zipMap[W](other: Map[K, W]): Map[K, (V, W)] = {
    val (m, o) = (map.filterKeys(other.keySet), other.filterKeys(map.keySet))
    m.map { case (key, value) => (key, (value, o(key)))}.toMap
  }
  def sorted(implicit ordering: Ordering[K]): SortedMap[K, V] = TreeMap.empty[K, V](ordering) ++ map
  def sortBy[S](f: K => S)(implicit ordering: Ordering[S]): SortedMap[K, V] = sorted(ordering.extendTo(f))
  def filterKeysNot(f: K => Boolean): Map[K, V] = map.filterKeys(!f(_))
  def filterValues(f: V => Boolean): Map[K, V] = map.filter(p => f(p._2))
  def filterValuesNot(f: V => Boolean): Map[K, V] = map.filter(p => !f(p._2))
  def forallValues(p: V => Boolean): Boolean = map.forall(kv => p(kv._2))
  def toExtractor = Extractor.from[K](map.get)
  def valueExists(p: V => Boolean): Boolean = map.exists(kv => p(kv._2))
  def difference(other: Map[K, V]): Map[K, V] = map.filterKeys(key => map.get(key) != other.get(key))
  def mapValuesEagerly[C](f: V => C): Map[K, C] = map.mapValues(f).toList.toMap
  def mutable: MMap[K, V] = MMap.empty[K, V] ++ map
  def partitionKeys(p: K => Boolean): (Map[K, V], Map[K, V]) = map.partition(kv => p(kv._1))
  def ifDefined[B](f: (Map[K, V] => B)): Option[B] = map.isEmpty ? none[B] | some(f(map))
}

class RichMultiMap[K, V](map : Map[K, List[V]]) extends RichMap[K, List[V]](map) {
  def contains(key : K, value : V) : Boolean = map.get(key).map(_.contains(value)).getOrElse(false)
  def contains(pair : (K, V)) : Boolean = contains(pair._1, pair._2)
  def allValues: List[V] = map.values.flatten.toList
  def union(k: K, v: List[V]): Map[K, List[V]] = map.getOrUpdate(k, old => (old ++ v).distinct)
  def union(kv: (K, List[V])): Map[K, List[V]] = union(kv._1, kv._2)
  def union(other: Map[K, List[V]]): Map[K, List[V]] = (map ++ other).mapValues(_.distinct)
}

class RichMutableMap[K, V](map: MMap[K, V]) {
  def findOrUpdate(p: ((K, V)) => Boolean, newEntry: => (K, V)): (K, V) = map.find(p).getOrElse(newEntry.update(map.update(_)))
  def update(kv: (K, V)) = map.update(kv._1, kv._2)
  def filterValues(f: V => Boolean): MMap[K, V] = map.filter(p => f(p._2))
  def remove(f : (K, V) => Boolean): MMap[K, V] = map.retain(f negate)
  def removeKeys(f: K => Boolean): MMap[K, V] = remove((k, V) => f(k))
  def removeValues(f: V => Boolean): MMap[K, V] = remove((k, v) => f(v))
  def retainKeys(f: K => Boolean): MMap[K, V] = map.retain((k, v) => f(k))
  def retainValues(f: V => Boolean): MMap[K, V] = map.retain((k, v) => f(v))
}

class MapView[K, V, C](map: Map[K, V], keyProjection: C => K) {
  def apply(key: C): V = map.apply(keyProjection(key))
  def get(key: C): Option[V] = map.get(keyProjection(key))
  def contains(key: C): Boolean = map.contains(keyProjection(key))
}

case class RichMapWithErrors[K : Manifest, V : Manifest](map : Map[K, V]) {
  val defaultMissingKeyExceptionMessage = "Missing key '%s' of type '%s', for values of type '%s'"
  def withException(s : String = defaultMissingKeyExceptionMessage) : Map[K, V] =
    map.withDefault(k => throw new java.util.NoSuchElementException(s.format(k.toString, manifest[K].erasure.getName, manifest[V].erasure.getName)))
}
object RichMapWithErrors {
  implicit def toRichMap[K : Manifest, V : Manifest](map : Map[K, V]) = RichMapWithErrors(map)
}