package starling.utils

import starling.utils.ImplicitConversions._
import scala.collection.mutable.{Map => MMap}
import collection.generic.Growable

object BidirectionalHashMap {
  def empty[K, V] = BidirectionalHashMap[K, V]()
}

case class BidirectionalHashMap[K, V](map: MMap[K, V] = MMap.empty[K, V], inverse: MMap[V, K] = MMap.empty[V, K])
  extends Growable[(K, V)] {

  def +=(other: (K, V)) = { map += other; inverse += other.swap; this }
  def clear() { map.clear; inverse.clear }

  def apply(key: K): V = map(key)
  def get(key: K): Option[V] = map.get(key)
  def getOrElseUpdate(key: K, f: => V) = map.getOrElseUpdate(key, f.update(inverse.getOrElseUpdate(_, key)))
  def reverse: BidirectionalHashMap[V, K] = BidirectionalHashMap(inverse, map)
}