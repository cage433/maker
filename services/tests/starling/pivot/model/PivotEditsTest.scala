package starling.pivot.model

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot.{SomeSelection, Field, KeyFilter, PivotEdits}

class PivotEditsTest extends TestNGSuite {

  @Test
  def testMultipleEditsDoNotResultInOverlappingEdits() {

    val twoFieldFilter = KeyFilter(Map(
      Field("Color") -> SomeSelection(Set("red")),
      Field("Height") -> SomeSelection(Set("2m"))
    ))
    val oneFieldFilter = KeyFilter(Map(
      Field("Color") -> SomeSelection(Set("red")),
      Field("Height") -> SomeSelection(Set("2m"))
    ))

    val oneThenTwo = PivotEdits.Null.withDelete(oneFieldFilter).withDelete(twoFieldFilter)
    val twoThenOne = PivotEdits.Null.withDelete(twoFieldFilter).withDelete(oneFieldFilter)
    val expected = PivotEdits.Null.withDelete(oneFieldFilter)
    assertEquals(oneThenTwo, expected)
    assertEquals(twoThenOne, expected)
  }

}