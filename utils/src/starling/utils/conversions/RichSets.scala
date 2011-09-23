package starling.utils.conversions


trait RichSets {
  implicit def enrichSet[T](set: Set[T]) = new RichSet(set)

  class RichSet[T](set: Set[T]) {
    def +++(other: Set[T]): Set[T] = other.foldLeft(set)(_ + _)
  }
}