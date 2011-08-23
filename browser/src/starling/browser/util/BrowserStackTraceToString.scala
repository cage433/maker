package starling.browser.util

import java.io.{PrintStream, ByteArrayOutputStream}

object BrowserStackTraceToString {
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
