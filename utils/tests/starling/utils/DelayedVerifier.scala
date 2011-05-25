package starling.utils

import org.mockito.Mockito._
import org.mockito.internal.verification.api.VerificationMode
import collection.mutable.ListBuffer


case class DelayedVerifier() {
  private val verifications : ListBuffer[() => Any] = ListBuffer()

  def apply() = verifications.foreach(v => v())
  def later[T](mock: T, mode: VerificationMode)(verification: T => Any) {
    verifications += (() => verification(verify(mock, mode)))
  }
}


