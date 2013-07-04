package maker.utils

trait Asserting{
  def asserting[T](x : T, check : T ⇒ Boolean, msg : String = "") = {
    assert(check(x), msg)
    x
  }
}
