package maker.utils

import scala.util.Either
import scala.util.Either.RightProjection
import scala.language.implicitConversions

trait EitherPimps {
  implicit def rightBias[L, R](either: Either[L, R]): RightProjection[L, R] = either.right

  implicit class RichEither[L, R](either : Either[L, R]){
    def andThen(nextEither : => Either[L, R]) = if (either.isLeft) either else nextEither
  }
}

object EitherPimps{

  def mapOrErrorLeft[A, L, R] (seq : Seq[A], fn : A => Either[L, R]) : Either[L, Seq[R]] = {
    val acc = new scala.collection.immutable.VectorBuilder[R]()
    var maybeFailure : Option[L] = None
    seq.foreach{
      a =>
        if (!maybeFailure.isDefined){
          fn(a) match {
            case Left(l) =>
              maybeFailure = Some(l)
            case Right(r) =>
              acc += r
          }
        }
    }
    maybeFailure.toLeft(acc.result)
  }
}

