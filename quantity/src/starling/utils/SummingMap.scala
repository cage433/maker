package starling.utils

import starling.quantity.Quantity
import collection.{Map, TraversableOnce, MapProxy}
import collection.generic.{TraversableFactory, MutableMapFactory, GenericCompanion, CanBuildFrom}
import collection.mutable.{HashMap, ArrayBuffer, Builder, MapLike}


class MutableListCollectingMap[K, V](val map : HashMap[K, List[V]] = new HashMap[K, List[V]]()){
  def -=(key: K) = {map -= key; this}

  def +=(kv: (K, V))  = {
    val (key, value) = kv
    map += key -> (value :: map.getOrElse(key, Nil))
    this
 }

  def iterator = map.iterator

  def get(key: K) = map.get(key)

}


/**
 * We seemed to often be netting quantities with respect to some property, so created this
 * class.
 * I didn't extend MapProxy as the '+' method is so different from that of Map that the
 * other methods don't behave as expected
 */
case class SummingMap[K](val underlying : scala.collection.Map[K, Quantity]) {
  def +(kv : (K, Quantity)) :SummingMap[K] = {
    val (key, value) = kv
    new SummingMap(underlying + ((key, underlying.getOrElse(key, Quantity.NULL) + value)))
  }

  def head = underlying.head
  def tail = new SummingMap[K](underlying.tail)

  def removeZeros = SummingMap(underlying.filterNot{case (k, v) => v.isAlmostZero})
  
  def ++(rhs : scala.collection.Map[K, Quantity]) = {
    var result = this
    rhs.foreach{
      case (k, v) => result = result + (k -> v)
    }
    result
  }

  override def toString = underlying.toString
  
  def apply(key : K) : Quantity = underlying.getOrElse(key, Quantity.NULL)
  
  def ++(other : SummingMap[K]) : SummingMap[K] = {
    (this /: other.underlying.iterator)(_+_)
  }

  def keySet = underlying.keySet
  def values = underlying.values
  def size = underlying.size

//  def *(x : Double) = SummingMap[K](Map.empty[K, Quantity] ++ mapValues(_*x))
  def *(x : Double) = new SummingMap[K](underlying.mapValues(_*x))

  def contains(key : K) = underlying.contains(key)
}

object SummingMap{
  def apply[K]() : SummingMap[K] = new SummingMap(Map.empty[K, Quantity])
  def empty[K]: SummingMap[K] = new SummingMap(Map.empty[K, Quantity]) 
}
