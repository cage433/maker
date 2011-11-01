package starling.utils.conversions

import starling.utils.ImplicitConversions._


trait RichTuple {
  implicit def enrichTuple2[A, B](t: (A,B)) = new RichTuple2[A, B](t)
  implicit def enrichTuple3[A, B, C](t: (A,B,C)) = new RichTuple3[A, B, C](t)
  implicit def enrichOrderedTuple2[L <: Ordered[L], R <: Ordered[R]]() = new Tuple2Ordering[L, R]()
  implicit def enrichOptionalMapTuple[K, V](t: (K, Option[V])) = new {
    def +:[W >: V](map: Map[K, W]) = t._2.map(v => map + t._1 → v).getOrElse(map)
  }
  implicit def enrichTuple2OfTuple2[A, B, C](t: ((A, B), C)) = new RichTuple2[(A, B), C](t) {
    def flatten = (t.head.head, t.head.tail, t.tail)
  }

  class RichTuple2[A, B](t: (A, B)) {
    def add[C](c: C) = (t._1, t._2, c)
    def head: A = t._1
    def tail: B = last
    def init: A = t._1
    def last: B = t._2

    def mapFirst[C](f: A => C): (C, B) = (f(t._1), t._2)
    def mapSecond[C](f: B => C): (A, C) = (t._1, f(t._2))
    def |>[C](f: (A, B) => C): C = f.tupled(t)

    def format(pattern: String) = pattern.format(t._1, t._2)
  }

  class RichTuple3[A, B, C](t: (A, B, C)) {
    def add[D](d: D) = (t._1, t._2, t._3, d)
    def head: A = t._1
    def tail: (B, C) = (t._2, t._3)
    def init: (A, B) = (t._1, t._2)
    def last: C = t._3

    def format(pattern: String) = pattern.format(t._1, t._2, t._2)
  }
}

class Tuple2Ordering[L <: Ordered[L], R <: Ordered[R]] extends Ordering[(L,R)] {
  def compare(x: (L,R), y: (L,R)) = x._1.compare(y._1) match {
    case 0 => x._2.compare(y._2)
    case other => other
  }
}