package starling.utils

import collection.generic.CanBuildFrom
import starling.utils.ImplicitConversions._
import collection.{IterableLike, TraversableLike, SeqLike}
import collection.mutable.{Map => MMap}
import scalaz.Scalaz._



trait CollectionsSyntacticSugar {
  class RichTraversableLike[+A, +Repr](tl : TraversableLike[A, Repr]) {
    def \ (x : Any) : Repr = tl.filterNot(_ == x)
    def \\ (seq : SeqLike[_, _]) = tl.filterNot(seq.contains(_))
    def quote(implicit bf: CanBuildFrom[Repr, String, Traversable[String]]) = tl.map(element => "'" + element + "'")

    def zipWith[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, (A, B), That]) = {
      val builder = cbf(tl.repr)

      tl.foreach(o => {
        val pair = (o, f(o))
        builder += pair
      })

      builder.result
    }

    def initOption() = if (tl.isEmpty) None else Some(tl.init)
    def dropUntil(p: A => Boolean): Repr = tl.dropWhile(!p(_))
    def takeUntil(p: A => Boolean): Repr = tl.takeWhile(!p(_))

    def flatMapO[B, That](f: A => Option[B])(implicit cbf: CanBuildFrom[Repr, B, That]) = tl.flatMap(a => f(a).toList)
    def safeMap[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, B, That]): That = flatMapO(f.option)
    def ifDefined[B](f: Repr => B): Option[B] = tl.isEmpty ? none[B] | some(f(tl.repr))
  }

  implicit def collectionExtras[A](xs: Iterable[A]) = new {
    def zipWith[B, C, That, Repr](ys: IterableLike[B, Repr])(f: (A, B) => C)(implicit cbf: CanBuildFrom[Repr, C, That]) = {
      val builder = cbf(ys.repr)
      val (i, j) = (xs.iterator, ys.iterator)
      while(i.hasNext && j.hasNext) {
        builder += f(i.next, j.next)
      }
      builder.result
    }
  }

  implicit def traversableLike2RichTraversableLike[A, Repr](tl: TraversableLike[A, Repr]) = new RichTraversableLike(tl)
  implicit def enrichTraversableLikeOfEithers[L, R, Repr](tl: TraversableLike[Either[L, R], Repr]) = new RichTraversableLike(tl) {
    def partitionEithers[LThat, RThat](implicit cbl: CanBuildFrom[Repr, L, LThat], cbr: CanBuildFrom[Repr, R, RThat]): (LThat, RThat) = {
      val (lbuilder, rbuilder)  = (cbl(tl.repr), cbr(tl.repr))

      tl.foreach(_.fold(lbuilder += _, rbuilder += _))

      (lbuilder.result, rbuilder.result)
    }
  }

  implicit def enrichTraversableLikeOfTuple2[A, B, Repr](tl: TraversableLike[(A, B), Repr]) = new RichTraversableLike(tl) {
    def mapFirst[C, That](f: A => C)(implicit cbf: CanBuildFrom[Repr, (C, B), That]): That = tl.map(_.mapFirst(f))
    def mapSecond[C, That](f: B => C)(implicit cbf: CanBuildFrom[Repr, (A, C), That]): That = tl.map(_.mapSecond(f))
    def _1[That](implicit cbf: CanBuildFrom[Repr, A, That]): That = tl.map(_._1)
    def _2[That](implicit cbf: CanBuildFrom[Repr, B, That]): That = tl.map(_._2)
    def swap[That](implicit cbf: CanBuildFrom[Repr, (B, A), That]): That = tl.map(_.swap)
    def toMutableMap: MMap[A, B] = MMap.empty[A, B] ++ tl

    def toMultiMap: MultiMap[A, B] = MMap.empty[A, List[B]].updateIt { map =>
      tl.foreach { case (key, value) => map.put(key, value :: map.getOrElse(key, List.empty[B])) }
    }.toMap
  }

  implicit def enrichNestedPairTraversableLIke[A, B, C, Repr](t: TraversableLike[(A, (B, C)), Repr]) = new RichTraversableLike(t) {
    def toNestedMap: NestedMap[A, B, C] = t.toMultiMap.mapValues(_.toMap)
  }
}