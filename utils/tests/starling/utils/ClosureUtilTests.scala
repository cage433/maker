package starling.utils

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test

class ClosureUtilTests extends TestNGSuite with ShouldMatchers {
  import ClosureUtil._

  @Test def shouldDecorateExceptions {
    intercept[Exception] {
      decorate(e => new IllegalArgumentException("Decorater")) {
        throw new IllegalArgumentException("Decoratee")
      }
    }.getMessage should be === "Decorater"
  }
}