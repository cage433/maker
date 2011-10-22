package starling.utils.conversions

import starling.utils.ImplicitConversions._


trait RichTraversables {
  implicit def enrichTraversable[A](traversable: Traversable[A]): RichTraversable[A] = new RichTraversable(traversable)
  // Convert List((1, 2), (1, 3), (2, 4)) to Map(1 → List(2, 3), 2 → List(4))
  implicit def enrichListsOfPairs[A, B](traversable : Traversable[(A, B)]) = new {
    def groupValues : Map[A, Traversable[B]] = traversable.groupInto(identity)
  }
}

class RichTraversable[A](traversable: Traversable[A]) {
  // TODO [08 Mar 2011] Replace with maxBy, minBy when we use scala 2.9
  def maximum[B](f: A => B)(implicit cmp: Ordering[B]): B = traversable.map(f).max
  def minimum[B](f: A => B)(implicit cmp: Ordering[B]): B = traversable.map(f).min
  def optMax(implicit cmd: Ordering[A]): Option[A] = traversable.ifDefined(_.max)
  def optMaxBy[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = traversable.ifDefined(_.maxBy(f))
  def mapDistinct[B](f: A => B): Set[B] = traversable.map(f).toSeq.distinct.toSet

  def findEnsureOnlyOne(p: A => Boolean) = traversable.filter(p) match {
    case e :: Nil => Some(e)
    case Nil => None
    case m => throw new Exception("Multiple instances match: " + m)
  }

  def toMapWithKeys[K](keyF: A => K):                  Map[K, A]      = pair(keyF).swap.toMap
  def toMapWithSomeKeys[K](keyF: A => Option[K]):      Map[K, A]      = optPair(keyF).swap.toMap
  def toMapWithManyKeys[K](keyF: A => List[K]):        Map[K, A]      = pairWithTraversable(keyF).swap.toMap
  def toMultiMapWithSomeKeys[K](keyF: A => Option[K]): MultiMap[K, A] = optPair(keyF).swap.toMultiMap
  def toMultiMapWithKeys[K](keyF: A => K):             MultiMap[K, A] = pair(keyF).swap.toMultiMap
  def toMapWithValues[V](valueF: A => V):              Map[A, V]      = pair(valueF).toMap
  def toMapWithSomeValues[V](valueF: A => Option[V]):  Map[A, V]      = optPair(valueF).toMap;

  def pair[B](f: A => B): Traversable[(A, B)] = traversable.map(_.pair(f))
  def pairWithTraversable[B](f: A => Traversable[B]): Traversable[(A, B)] = traversable.flatMap(_.pairWithTraversable(f))
  def pair[B](b: B): Traversable[(A, B)] = traversable.map(_ → b)
  def optPair[B](f: A => Option[B]): Traversable[(A, B)] = traversable.flatMap(_.optPair(f))

  def groupInto[B, C](keyF: A => B, valueF: A => C): Map[B, Traversable[C]] = {
    traversable.groupBy(keyF).mapValues(_.map(valueF))
  }

  def groupInto[B, C](keyAndValueF: A => (B, C)): Map[B, Traversable[C]] = {
    traversable.map(keyAndValueF).groupBy(_._1).mapValues(_.map(_._2))
  }

  def toMultiMapPF[B, C](pf: PartialFunction[A, (B, C)]): MultiMap[B, C] = traversable.flatMapO(pf.lift).toMultiMap

  def maxOr(alternative: => A)(implicit cmp: Ordering[A]) = if (traversable.isEmpty) alternative else traversable.max
}

