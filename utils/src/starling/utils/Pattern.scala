package starling.utils

import ImplicitConversions._
import scalaz.Scalaz._


object Pattern extends Log {
  case class Extractor[A, B](f: A => Option[B]) {
    def unapply(a: A): Option[B] = try { f(a) } catch { case e => log.warnF("Exception in Extractor", e)(None) }
    def unapply[C](ta: Traversable[A])(implicit g: Flattener[B, C]): Option[C] = g(ta.view.map(f))

    def compose[C](g: C => A) = new Extractor[C, B](f compose g)
    def filter(p: B => Boolean): Extractor[A, B] = andThen[B]((b: B) => p(b) option(b))
    def andThen[C](g: B => Option[C]): Extractor[A, C] = new Extractor[A, C]((a:A) => f(a).flatMap(g))
    def andThen[C](g: Extractor[B, C]): Extractor[A, C] = andThen(g.f)
    def orElse(alternative: Extractor[A, B]): Extractor[A, B] = orElse(alternative.f)
    def orElse(alternative: A => Option[B]): Extractor[A, B] = new Extractor[A, B](a => f(a).orElse(alternative(a)))

    object Pick {
      def unapply(ta: Traversable[A]) = pick(ta)(f)
    }
  }

  object Extractor {
    def from[A] = new {
      def apply[B](f: A => Option[B]) = new Extractor(f)
    }

    def when[A] = new {
      def apply(f: A => Boolean) = from[A]((a:A) => f(a).option(a))
      def apply[B](f: A => Boolean, g: A => B) = from[A]((a:A) => f(a).option(g(a)))
    }

    def map[A] = new {
      def apply[B](f: A => B) = new Extractor((a:A) => Some(f(a)))
    }

    def regex[A](regex: String) = new {
      def apply(pf: PartialFunction[List[String], A]): Extractor[String, A] = from[String](s => regex.r.unapplySeq(s).flatMap(pf.lift(_)))
    }

    def fromMap[K, V](map: Map[K, V]) = Extractor.from[K](map.get)
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

  val NestedException: Extractor[Throwable, (Throwable, Throwable)] =
    Extractor.from[Throwable](t => t.optPair(t.getCause.safeCast[Throwable]))
}