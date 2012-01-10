package starling.utils.conversions

import org.scalatest.matchers.ShouldMatchers
import starling.utils.ImplicitConversions._
import org.scalatest.WordSpec


class RichAnyTests extends WordSpec with ShouldMatchers {
  "should apply functions in sequence" in {
    1.applyAll(_ + 1, _ * 2) should be === 4
    1.applyAll(_ * 2, _ + 1) should be === 3
    "Hello".applyAll(_ + ", ", _ + "world!") should be === "Hello, world!"
  }
}