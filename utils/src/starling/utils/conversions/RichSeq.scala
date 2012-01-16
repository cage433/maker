package starling.utils.conversions
import shapeless.Typeable


trait RichSeq {
  implicit def enrichSeq[T](seq: Seq[T]) = new RichSeq(seq)
  class RichSeq[T](seq: Seq[T]) extends (T => Boolean) {
    def apply(elem: T) = seq.contains(elem)
    def contentsOr(alternative: T*): Seq[T] = if (seq.isEmpty) alternative else seq
    def contentsOr(alternative: List[T]): Seq[T] = if (seq.isEmpty) alternative else seq
    def filterCast[T](implicit t: Typeable[T]): Seq[T] = seq.flatMap(t.cast(_))
    def find(pf: PartialFunction[T, Boolean]): Option[T] = seq.find(value => pf.lift(value).getOrElse(false))
  }
}