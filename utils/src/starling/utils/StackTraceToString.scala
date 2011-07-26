package starling.utils

import java.io.{ByteArrayOutputStream, PrintStream}

object StackTraceToString {
  def apply(throwable : Throwable) = string(throwable)
  
  def string(throwable : Throwable) = {
    val stream = new ByteArrayOutputStream()
		throwable.printStackTrace(new PrintStream(stream))
		stream.toString
  }

  def messageAndThenString(throwable:Throwable) = {
    throwable.getMessage + "\n\n" + string(throwable)
  }
}
