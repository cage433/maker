package starling.pivot.model

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot._

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

  @Test
  def testRemoveEdits() {

    val editKey = PivotEdits.Null.withAmend(KeyFilter(Map(Field("Trader")->SomeSelection(Set("t")), Field("Market") -> SomeSelection(Set("m1")))), Field("Market"), Some("m2"))
    val editMeasure = PivotEdits.Null.withAmend(KeyFilter(Map(Field("Trader")->SomeSelection(Set("t")), Field("Market") -> SomeSelection(Set("m2")))), Field("Volume"), Some("3"))

    val combined = editMeasure addEdits editKey

    assertEquals( combined remove editKey, editMeasure)
    assertEquals( combined remove editMeasure, editKey)
    assertEquals( (combined remove editKey) remove editMeasure, PivotEdits.Null)
    assertEquals( (combined remove editMeasure) remove editKey, PivotEdits.Null)
  }

}