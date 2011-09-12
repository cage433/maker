package starling.utils.conversions

import java.io.{PrintWriter, StringWriter}

import starling.utils.ImplicitConversions._

trait RichThrowables {
  implicit def enrichThrowable[T <: Throwable](throwable: T) = new RichThrowable(throwable)
}

class RichThrowable[T <: Throwable](throwable: T) {
  def stackTraceAsString = new StringWriter().update(writer => throwable.printStackTrace(new PrintWriter(writer))).toString
}