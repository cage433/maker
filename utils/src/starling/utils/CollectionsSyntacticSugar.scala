package starling.utils

import collection.generic.CanBuildFrom
import starling.utils.ImplicitConversions._
import collection.{IterableLike, TraversableLike, SeqLike}
import collection.mutable.ListBuffer


trait CollectionsSyntacticSugar {
  class RichTraversableLike[+A, +Repr](tl : TraversableLike[A, Repr]){
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

    def initOption() = {
      if (tl.isEmpty) None else Some(tl.init)
    }
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
    def split[LThat, RThat](implicit cbl: CanBuildFrom[Repr, L, LThat], cbr: CanBuildFrom[Repr, R, RThat]): (LThat, RThat) = {
      val lbuilder = cbl(tl.repr)
      val rbuilder = cbr(tl.repr)

      tl.foreach(o => o match {
        case Left(l) => lbuilder += l
        case Right(r) => rbuilder += r
      })

      (lbuilder.result, rbuilder.result)
    }
  }
}