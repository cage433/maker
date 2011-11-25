package starling.utils

import starling.utils.ImplicitConversions._
import starling.utils.ClosureUtil._


class RetryingAction[T](action: () => T) extends (() => T) {
  private var result: Option[Either[Throwable, T]] = None

  def apply(): T = synchronized {
    result match {
      case Some(Right(value)) =>
      case _ => result = Some(safely(action()))
    }

    result.get.getOrThrow
  }
}