package maker.utils

import scala.util.Either
import scala.util.Either.RightProjection

trait EitherPimps {
  implicit def rightBias[L, R](either: Either[L, R]): RightProjection[L, R] = either.right

  implicit class RichEither[L, R](either : Either[L, R]){
    def andThen(nextEither : => Either[L, R]) = if (either.isLeft) either else nextEither
  }
}

