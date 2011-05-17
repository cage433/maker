package starling.utils

object AssertionUtil {
  def fail[A](message : String) : A = throw new java.lang.AssertionError(message)
}