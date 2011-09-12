package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import collection.immutable.Map

class RichMapTests extends TestNGSuite with ShouldMatchers with RichMaps {
  val nothing: Map[Int, String] = Map[Int, String]()
  val map = Map(1 → "2")
  val mapWithDifferentValue = Map(1 → "3")

  @Test def aMapMinusItselfEqualsNothing {
    map - map should be === nothing
    nothing - nothing should be === nothing
  }

  @Test def aMapMinusNothingEqualsItself {
    map - nothing should be === map
  }

  @Test def nothingMinusAMapEqualsNothing {
    nothing - map should be === nothing
  }

  @Test def aMapMinusAMapWithADifferentValueShouldEqualItself {
    map - mapWithDifferentValue should be === map
  }
}