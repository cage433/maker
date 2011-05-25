package starling.utils.conversions

import starling.utils.{ImplicitConversions, Log}


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
        ordering.compare(x, y).debug { result => "compare(%s, %s) = %d" % (x, y, result) }
      }
    }

    def named(name : String) = new NamedOrdering[T](name, ordering) {
      def compare(x: T, y: T) = ordering.compare(x, y)
    }

    abstract class NamedOrdering[A](name : String, ordering : Ordering[_]) extends Ordering[A] {
      override def toString = "%s (%s)" % (name, ordering.toString)
    }
  }
}

