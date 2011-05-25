package starling.utils


import java.io.{ByteArrayOutputStream, PrintStream}

object StackTraceToString {
  def apply(throwable : Throwable) = string(throwable)
  
  def string(throwable : Throwable) = {
    val stream = new ByteArrayOutputStream()
		throwable.printStackTrace(new PrintStream(stream))
		stream.toString()
  }
}
