package starling.utils.conversions
import starling.utils.ImplicitConversions._


trait RichSeq {
  implicit def enrichSeq[T](seq: Seq[T]) = new {
    def contentsOr(alternative: T*): Seq[T] = if (seq.isEmpty) alternative else seq
    def contentsOr(alternative: List[T]): Seq[T] = if (seq.isEmpty) alternative else seq

    def filterCast[T](implicit m : Manifest[T]): Seq[T] = seq.flatMap(m.cast(_))
  }
}