package starling.utils.conversions
import starling.utils.ImplicitConversions._


trait RichSeq {
  implicit def enrichSeq[T](seq: Seq[T]) = new RichSeq(seq)
  class RichSeq[T](seq: Seq[T]) extends (T => Boolean) {
    def apply(elem: T) = seq.contains(elem)
    def contentsOr(alternative: T*): Seq[T] = if (seq.isEmpty) alternative else seq
    def contentsOr(alternative: List[T]): Seq[T] = if (seq.isEmpty) alternative else seq
    def filterCast[T](implicit m : Manifest[T]): Seq[T] = seq.flatMap(m.safeCast(_))
    def find(pf: PartialFunction[T, Boolean]): Option[T] = seq.find(value => pf.lift(value).getOrElse(false))
  }
}