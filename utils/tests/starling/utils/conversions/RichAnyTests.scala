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

  "equalTo" in {
    class Thing(val name: String) {
      override def equals(other: Any): Boolean = other.equalTo[Thing](_.name == name)
    }

    class Widget(val name: String, val size: Int) {
      override def equals(other: Any): Boolean = other.equalTo[Widget](_.name == name, _.size == size)
    }

    new Thing("a") should be === new Thing("a")
    new Thing("a") should not be === (new Thing("b"))
    new Widget("w", 1) should be === new Widget("w", 1)
    new Widget("w", 1) should not be === (new Widget("w", 2))
    new Widget("w", 1) should not be === (new Widget("x", 1))
  }
}