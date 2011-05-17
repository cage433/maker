package starling.utils


trait OrderedComparable[T] extends Ordered[T] with Comparable[T] {
  override def compareTo(that: T) = compare(that)
}
