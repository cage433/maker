package test.scalaz

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import scalaz.Scalaz._


class MASugarTests extends WordSpec with ShouldMatchers {
  "test Scalaz Cartesian Product" in {
    case class Thing(a: Int, b: Int, c: Int)

    val things = List(1, 2) ⊛ List(30, 40) ⊛ List(500, 600) apply(Thing.apply)

    things should be === List(
      Thing(1, 30, 500), Thing(1, 30, 600), Thing(1, 40, 500), Thing(1, 40, 600),
      Thing(2, 30, 500), Thing(2, 30, 600), Thing(2, 40, 500), Thing(2, 40, 600)
    )
  }
}

