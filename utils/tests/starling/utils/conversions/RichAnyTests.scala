package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import starling.utils.ImplicitConversions._
import scalaz._
import Scalaz._
import starling.utils.Log


class RichAnyTests extends TestNGSuite with ShouldMatchers {
  @Test def shouldApplyFunctionsInSequence {
    1.applyAll(v => v + 1, v => v * 2) should be === 4
    1.applyAll(v => v * 2, v => v + 1) should be === 3
    "Hello".applyAll(v => v + ", ", v => v + "world!") should be === "Hello, world!"
  }

  @Test def testScalazCartesianProduct {
    case class Thing(a: Int, b: Int, c: Int)

    val things = List(1, 2) ⊛ List(30, 40) ⊛ List(500, 600) apply(Thing.apply)

    things should be === List(
      Thing(1, 30, 500), Thing(1, 30, 600), Thing(1, 40, 500), Thing(1, 40, 600),
      Thing(2, 30, 500), Thing(2, 30, 600), Thing(2, 40, 500), Thing(2, 40, 600)
    )
  }
}