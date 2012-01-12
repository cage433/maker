package starling.utils

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class RichStringTests extends WordSpec with ShouldMatchers with RichString {
  "can replace using map" in {
    "foobar".replaceAll(Map("foo" → "oof", "bar" → "rab")) should be === "oofrab"
  }
}