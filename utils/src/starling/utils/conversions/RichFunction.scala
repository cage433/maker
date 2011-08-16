package starling.utils.conversions

import scala.util.control.Exception._


trait RichFunction {
  implicit def enrichFunction2[A, B](f: A => B) = new RichFunction2(f)
  class RichFunction2[A, B](f: A => B) extends PartialFunction[A, B] {
    def isDefinedAt(x: A) = true
    def apply(a: A) = f(a)
    def ***[C, D](pf: PartialFunction[C, D]) = enrichPartialFunction(this) *** pf
    def either: A => Either[Throwable, B] = (a: A) => catching(classOf[Exception]) either(f(a))
    def option: A => Option[B] = (a: A) => catching(classOf[Exception]) opt (f(a))
  }
  implicit def enrichFunction3[A, B, C, R](f: (A, B, C) => R) = new RichFunction3(f)
  implicit def enrichFunction4[A, B, C, D, R](f: (A, B, C, D) => R) = new RichFunction4(f)

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
