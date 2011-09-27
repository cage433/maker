package starling.utils.conversions

import starling.utils.ImplicitConversions._


trait RichEither {
  implicit def enrichEither[L, R](either: Either[L, R]) = new RichEither(either)

  implicit def enrichExceptionalEither[L <: Throwable, R](either: Either[L, R]) = new RichEither(either) {
    def printStackTrace = update(_.printStackTrace, identity)
    def getOrThrow = either.fold(throw _, identity)
  }

  implicit def enrichFailureEither[R](either: Either[Failure, R]) = new RichFailureEither(either)

  implicit def enrichOrderedEither[L <: Ordered[L], R <: Ordered[R]](self: Either[L, R]) = new RichEither(self) {
    def compareEither(that: Either[L, R]) = (self, that) match {
      case (Left(selfL), Left(thatL)) => selfL.compare(thatL)
      case (Right(selfR), Right(thatR)) => selfR.compare(thatR)
      case (Left(_), Right(_)) => 1
      case (Right(_), Left(_)) => -1
    }
  }

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
    def mapLeft[L2](l: L => L2): Either[L2, R] = map(l, identity)
    def mapRight[R2](r: R => R2): Either[L, R2] = map(identity, r)
    def map[LV, RV](left: L => LV, right: R => RV): Either[LV, RV] = either.fold(l => Left(left(l)), r => Right(right(r)))
    def update[LV, RV](left: L => LV, right: R => RV): Either[L, R] = { either.fold(left, right); either }
    def toOption = either.right.toOption
    def getOrElse(alternative: R) = toOption.getOrElse(alternative)
    def orElse(alternative: Option[R]) = toOption.orElse(alternative)
  }
}