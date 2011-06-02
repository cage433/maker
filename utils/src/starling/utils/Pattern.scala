package starling.utils

import ImplicitConversions._


object Pattern {
  case class Extractor[A, B](f: A => Option[B]) {
    def unapply(a: A) = f(a)
    def unapply[C](ta: Traversable[A])(implicit g: Flattener[B, C]): Option[C] = g(ta.view.map(f))

    object Pick {
      def unapply(ta: Traversable[A]) = pick(ta)(f)
    }
  }

  object Extractor {
    def from[A] = new From[A]
    def when[A] = new {
      def apply(f: A => Boolean) = from[A]((a:A) => f(a).toOption(a))
    }

    class From[A] {
      def apply[B](f: A => Option[B]) = new Extractor[A, B](f)

      def partial = new {
        def apply[B](pf: PartialFunction[A, B]) = new Extractor[A, B](pf.lift)
      }
    }
  }

  case class Tester[A](p: A => Boolean) {
    def unapply(a: A) = p(a)
    def unapply(t: Traversable[A]) = t forall p

    object Exists {
      def unapply(t: Traversable[A]) = t exists p
    }
  }

  object & {
    def unapply[A](a: A) = Some(a, a)
  }

  object Select {
    def unapply[A](t : Traversable[A]) = t.headOption
  }

  object Single {
    def unapply[A](t: Traversable[A]) = if(t.size == 1) Some(t.head) else None
  }

  trait Flattener[B, C] extends (Traversable[Option[B]] => Option[C])

  class DefaultFlattener {
    implicit def defaultFlattener[B] = new Flattener[B, Traversable[B]] {
      def apply(tb: Traversable[Option[B]]): Option[Traversable[B]] = nonEmpty(tb.flatten)
    }
  }

  object Flattener extends DefaultFlattener {
    implicit def flattenSets[E] = new Flattener[Set[E], Set[E]] {
      def apply(tb: Traversable[Option[Set[E]]]): Option[Set[E]] = nonEmpty(tb.flatten.flatten.toSet)
    }
  }

  def nonEmpty[T <: Traversable[_]](t: T )= if(t isEmpty) None else Some(t)

  def pick[A, B](ta: Traversable[A])(f: A => Option[B]): Option[B] = {
    for(a <- ta) {
      val b = f(a)
      if(b.isDefined) {
        return b
      }
    }

    None
  }

  val NestedException = Extractor.from[Throwable](t => t.optPair(t.getCause.safeCast[Throwable]))
}