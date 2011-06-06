package starling.utils.conversions

import starling.utils.ImplicitConversions._
import collection.mutable.HashMap


trait RichTraversable {
  implicit def enrichTraversable[A](traversable: Traversable[A]) = new RichTraversable(traversable)
  implicit def enrichPairTraversable[A, B](traversable: Traversable[(A, B)]) = new RichTraversable(traversable) {
    def toMultiMap: Map[A, Set[B]] = HashMap[A, Set[B]]().updateIt { map =>
      traversable.foreach { case (key, value) => map.put(key, (map.getOrElse(key, Set[B]()) + value)) }
    }.toMap
    def swap: Traversable[(B, A)] = traversable.map(_.swap)
  }

  class RichTraversable[A](traversable: Traversable[A]) {
    // TODO [8 Mar 2011]: Replace with maxBy, minBy when we use scala 2.9
    def maximum[B](f: A => B)(implicit cmp: Ordering[B]): B = traversable.map(f).max
    def minimum[B](f: A => B)(implicit cmp: Ordering[B]): B = traversable.map(f).min
    def mapDistinct[B](f: A => B): Set[B] = traversable.map(f).toSeq.distinct.toSet

    def toMapWithKeys[K](keyF: A => K):                  Map[K, A]      = pair(keyF).swap.toMap
    def toMapWithSomeKeys[K](keyF: A => Option[K]):      Map[K, A]      = optPair(keyF).swap.toMap
    def toMultiMapWithSomeKeys[K](keyF: A => Option[K]): Map[K, Set[A]] = optPair(keyF).swap.toMultiMap
    def toMapWithValues[V](valueF: A => V):              Map[A, V]      = pair(valueF).toMap
    def toMapWithSomeValues[V](valueF: A => Option[V]):  Map[A, V]      = optPair(valueF).toMap;

    def pair[B](f: A => B): Traversable[(A, B)] = traversable.map(_.pair(f))
    def pair[B](b: B): Traversable[(A, B)] = traversable.map(_ → b)
    def optPair[B](f: A => Option[B]): Traversable[(A, B)] = traversable.flatMap(_.optPair(f))

    def groupInto[B, C](keyF: A => B, valueF: A => C): Map[B, Traversable[C]] = {
      traversable.groupBy(keyF).mapValues(_.map(valueF))
    }

    def groupInto[B, C](keyAndValueF: A => (B, C)): Map[B, Traversable[C]] = {
      traversable.map(keyAndValueF).groupBy(_._1).mapValues(_.map(_._2))
    }
  }
}

