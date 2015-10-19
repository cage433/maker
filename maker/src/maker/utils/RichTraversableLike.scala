package maker.utils

import scala.collection.TraversableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions

object RichTraversableLike{
  class RichTraversableLike[+A, +Repr](tl : TraversableLike[A, Repr]) {
    def \ (x : Any) : Repr = tl.filterNot(_ == x)
    def \\ (seq : SeqLike[_, _]) = tl.filterNot(seq.contains(_))
    def quote(implicit bf: CanBuildFrom[Repr, String, Traversable[String]]) = tl.map(element => "'" + element + "'")

    /** Zip every element if the TraversableLike with a constant element 'b'*/
    def zipWith[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, (A, B), That]): That = tl.map(a => (a, f(a)))
    /** Pair every element if the TraversableLike with a constant element 'b'*/
    def pairWith[B, That](b: B)(implicit cbf: CanBuildFrom[Repr, (A, B), That]): That = tl.map(_ -> b)

    def initOption(): Option[Repr] = if (tl.isEmpty) None else Some(tl.init)

    /** Apply the function 'f' if the TraversableLike is nonEmpty */
    //def ifDefined[B](f: Repr => B): Option[B] = tl.isEmpty ? none[B] | some(f(tl.repr))

    def partitionEithers[B, C, LThat, RThat](f: A => Either[B, C])(
      implicit cbl: CanBuildFrom[Repr, B, LThat], cbr: CanBuildFrom[Repr, C, RThat]): (LThat, RThat) = { 

      val (lbuilder, rbuilder) = (cbl(tl.repr), cbr(tl.repr))

      tl.foreach(f(_).fold(lbuilder += _, rbuilder += _)) 

      (lbuilder.result(), rbuilder.result())
    }   
  }
  implicit def traversableLike2RichTraversableLike[A, Repr](tl: TraversableLike[A, Repr]) = new RichTraversableLike(tl)
}   
