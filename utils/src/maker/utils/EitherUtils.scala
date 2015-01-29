package maker.utils

import scala.util.Either
import scala.util.Either.RightProjection

trait EitherUtils {
  implicit def rightBias[L, R](either: Either[L, R]): RightProjection[L, R] = either.right
}

