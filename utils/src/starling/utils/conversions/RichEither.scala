package starling.utils.conversions

import starling.utils.ImplicitConversions._


trait RichEither {
  implicit def enrichExceptionalEither[L <: Throwable, R](either: Either[L, R]) = new RichEither(either) {
    def printStackTrace = update(throwable => throwable.printStackTrace, identity)
    def getOrThrow = either.fold(left => throw left, right => right)
  }

  implicit def enrichFailureEither[R](either: Either[Failure, R]) = new RichFailureEither(either)

  class RichFailureEither[R](either: Either[Failure, R]) extends RichEither[Failure, R](either) {
    def orElse(alternative: Either[Failure, R]): Either[Failure, R] = either match {
      case Left(failure) => alternative.map(alternativeFailure => failure + alternativeFailure, identity)
      case _ => either
    }
    def getOrThrow = either.fold(_.throwException, identity)
  }

  case class Failure(messages: String*) {
    def +(failure: Failure) = this
    def throwException = throw new Exception(messages.mkString(", "))
  }

  class RichEither[L, R](either: Either[L, R]) {
    def map[LV, RV](left: L => LV, right: R => RV): Either[LV, RV] = either.fold(l => Left(left(l)), r => Right(right(r)))
    def update[LV, RV](left: L => LV, right: R => RV): Either[L, R] = { either.fold(left, right); either }
    def toOption = either.right.toOption
    def getOrElse(alternative: R) = toOption.getOrElse(alternative)
    def orElse(alternative: Option[R]) = toOption.orElse(alternative)
  }
}