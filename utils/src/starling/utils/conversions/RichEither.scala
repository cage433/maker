package starling.utils.conversions


trait RichEither {
  implicit def enrichExceptionalEither[L <: Exception, R](either: Either[L, R]) = new ExceptionalEither[L, R](either)

  class ExceptionalEither[L <: Exception, R](either: Either[L, R]) {
    def update[LV, RV](left: L => LV, right: R => RV): Either[L, R] = { either.fold(left, right); either }
    def printStackTrace = update(exception => exception.printStackTrace, identity)
  }
}