package starling.utils.conversions

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class RichOrderingTests extends WordSpec with ShouldMatchers with RichOrdering {
  "lexicographicalOrdering works" in {
    val animals@List(zebra, aardvark, cat) = List(Animal("Zebra", 4), Animal("Aardvark", 12), Animal("Cat", 3))

    animals.sorted(lexicographicalOrdering[Animal](_.name, _.age)) should be === List(aardvark, cat, zebra)
    animals.sorted(lexicographicalOrdering[Animal](_.age, _.name)) should be === List(cat, zebra, aardvark)
  }

  private case class Animal(name: String, age: Int)
}