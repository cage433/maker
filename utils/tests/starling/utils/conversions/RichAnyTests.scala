package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import starling.utils.ImplicitConversions._


class RichAnyTests extends TestNGSuite with ShouldMatchers {
  @Test def shouldApplyFunctionsInSequence {

    1.applyAll(v => v + 1, v => v * 2) should be === 4
    1.applyAll(v => v * 2, v => v + 1) should be === 3
    "Hello".applyAll(v => v + ", ", v => v + "world!") should be === "Hello, world!"
  }
}