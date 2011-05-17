package starling.utils.conversions


trait RichFunction {
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
}
