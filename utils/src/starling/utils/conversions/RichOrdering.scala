package starling.utils.conversions

import starling.utils.{ImplicitConversions, Log}
import scalaz.Scalaz._


trait RichOrdering {
  implicit def enrichOrdering[T](ordering : Ordering[T]) = new RichOrdering[T](ordering)

  class RichOrdering[T](ordering : Ordering[T]) {
    import ImplicitConversions._

    val untyped = new NamedOrdering[Any]("untyped", ordering) {
      def compare(x: Any, y: Any) = {
        ordering.compare(x.asInstanceOf[T], y.asInstanceOf[T])
      }
    }

    val asserting = new NamedOrdering[T]("asserting", ordering) {
      def compare(x: T, y: T) = {
        ordering.compare(x, y).deny(c => c == 0 && x != y, "comparison == 0 should imply that " + x + " == " + y)
      }
    }

    val debug = if (!Log.isDebugEnabled) ordering else new NamedOrdering[T]("debug", ordering) {
      def compare(x: T, y: T) = {
        ordering.compare(x, y).debugV { result => "compare(%s, %s) = %d" % (x, y, result) }
      }
    }

    def named(name : String) = new NamedOrdering[T](name, ordering) {
      def compare(x: T, y: T) = ordering.compare(x, y)
    }

    def contraMap[S](f: S => T): Ordering[S] = ordering contramap(f)

    abstract class NamedOrdering[A](name : String, ordering : Ordering[_]) extends Ordering[A] {
      override def toString = "%s (%s)" % (name, ordering.toString)
    }
  }

  def lexicographicalOrdering[T](orderings: Ordering[T]*): Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = orderings.foldLeft(0)((res, ordering) => (res == 0) ? 0 | ordering.compare(x, y))
  }

  class OrderedOrdering[T <: Ordered[T]] extends Ordering[T] {
    def compare(x: T, y: T) = x.compare(y)
  }
}

