package starling.utils.conversions

import scala.util.control.Exception._
import starling.utils.Stopwatch
import starling.utils.ImplicitConversions._


trait RichFunction {
  implicit def enrichFunction1[A, B](f: A => B) = new RichFunction1(f)

  class RichFunction1[A, B](f: A => B) extends PartialFunction[A, B] {
    def isDefinedAt(x: A) = true
    def apply(a: A) = f(a)
    def ***[C, D](pf: PartialFunction[C, D]) = enrichPartialFunction(this) *** pf
    def either: A => Either[Throwable, B] = (a: A) => catching(classOf[Exception]) either(f(a))
    def option: A => Option[B] = (a: A) => catching(classOf[Exception]) opt (f(a))
    def withTime: (A) => (B, Stopwatch) = (a: A) => new Stopwatch().apply(stopwatch => f(a) → stopwatch)
  }

  implicit def enrichPredicate[A](p: A => Boolean) = new RichFunction1(p) {
    def &&(q: A => Boolean): (A) => Boolean = (a: A) => p(a) && q(a)
    def negate: (A) => Boolean = (a: A) => !(p(a))
  }

  implicit def enrichPredicateOfTuple2[A, B](p : (A, B) => Boolean) = new RichFunction2(p) { // For negating functions on maps
    def &&(q: (A, B) => Boolean): (A, B) => Boolean = (a: A, b: B) => p(a, b) && q(a, b)
    def negate: (A, B) => Boolean = (a: A, b: B) => !(p(a, b))
  }

  implicit def enrichFunction3[A, B, C, R](f: (A, B, C) => R) = new RichFunction3(f)
  implicit def enrichFunction4[A, B, C, D, R](f: (A, B, C, D) => R) = new RichFunction4(f)

  class RichFunction2[A, B, R](f: (A, B) => R) extends (((A, B)) => R) {
    def apply(t: (A, B)) = f.tupled(t)
  }

  class RichFunction3[A, B, C, R](f: (A, B, C) => R) extends (((A, B, C)) => R) {
    def apply(t: (A, B, C)): R = f.tupled(t)
    def applyLast(c: C): (A, B) => R = (a, b) => f(a, b, c)
  }

  class RichFunction4[A, B, C, D, R](f: (A, B, C, D) => R) extends (((A, B, C, D)) => R) {
    def apply(t: (A, B, C, D)) = f.tupled(t)
    def applyLast(d: D): (A, B, C) => R = (a, b, c) => f(a, b, c, d)
  }

  implicit def enrichPartialFunction[A, B](pf1: PartialFunction[A, B]) = new {
    def ***[C, D](pf2: PartialFunction[C, D]) = new PartialFunction[(A, C), (B, D)] {
      def isDefinedAt(pair: (A, C)) = pf1.isDefinedAt(pair._1) && pf2.isDefinedAt(pair._2)
      def apply(pair: (A, C)) = (pf1.apply(pair._1), pf2.apply(pair._2))
    }
  }
}
