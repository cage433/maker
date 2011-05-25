package starling.utils


trait RichCollection {
  import collection.mutable._
  import scala.collection.IterableLike
  import scala.collection.generic.CanBuildFrom

  class RichCollection[A, Repr](xs: IterableLike[A, Repr]) {
    def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
      val builder = cbf(xs.repr)
      val i = xs.iterator
      var set = Set[B]()
      while (i.hasNext) {
        val o = i.next
        val b = f(o)
        if (!set(b)) {
          set += b
          builder += o
        }
      }
      builder.result
    }
  }

  implicit def toRich[A, Repr](xs: IterableLike[A, Repr]) = new RichCollection(xs)
}


