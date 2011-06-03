package starling.utils.conversions

import collection.mutable.ListBuffer


trait RichTraversable {
  implicit def enrichTraversable[A](traversable: Traversable[A]) = new RichTraversable(traversable)

  class RichTraversable[A](traversable: Traversable[A]) {
    // TODO: Replace with maxBy, minBy when we use scala 2.9
    def maximum[B](f: A => B)(implicit cmp: Ordering[B]): B = traversable.map(f).max
    def minimum[B](f: A => B)(implicit cmp: Ordering[B]): B = traversable.map(f).min
    def mapDistinct[B](f: A => B): Set[B] = traversable.map(f).toSeq.distinct.toSet
    def toMapWithKeys[K](keyF: A => K): Map[K, A] = traversable.map(i => (keyF(i), i)).toMap
    def toMapWithSomeKeys[K](keyF: A => Option[K]): Map[K, A] = traversable.flatMap(i => keyF(i).map(_ → i)).toMap
    def toMapWithValues[V](valueF: A => V): Map[A, V] = traversable.map(i => (i, valueF(i))).toMap
    def toMapWithSomeValues[V](valueF: A => Option[V]): Map[A, V] = traversable.flatMap(i => valueF(i).map(i → _)).toMap;
    def pair[B](f: A => B): Traversable[(A,B)] = traversable.map(elem => (elem, f(elem)))
    def optPair[B](f: A => Option[B]) = traversable.flatMap(elem => f(elem).map(pair => (elem, pair)))

    def groupInto[B, C](keyF: A => B, valueF: A => C): Map[B, Traversable[C]] = {
      traversable.groupBy(keyF).mapValues(_.map(valueF))
    }

    def groupInto[B, C](keyAndValueF: A => (B, C)): Map[B, Traversable[C]] = {
      traversable.map(keyAndValueF).groupBy(_._1).mapValues(_.map(_._2))
    }
  }
}

