package starling.pivot

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import scalaz.Scalaz._


class RowTests extends WordSpec with ShouldMatchers {
  private val row = Row(Field("string") → "foo", Field("double") → 3.14)

  "Should be able to extract typed values from map" in {
    row[String](Field("string")) should be === "foo"
    row[Double](Field("double")) should be === 3.14
  }

  "Should be able to add entries to Row" in {
    Row(Field("string") → "foo") + (Field("double") → 3.14) should be === row
  }
}