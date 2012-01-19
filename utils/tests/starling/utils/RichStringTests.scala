package starling.utils

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class RichStringTests extends WordSpec with ShouldMatchers with RichString {
  "can replace using map" in {
    "foobar".replaceAll(Map("foo" → "oof", "bar" → "rab")) should be === "oofrab"
  }

  "%" in {
    val tuple = ("x", "y")
    ("x: %s, y: %s" % ("x", "y")) should be === "x: x, y: y"
    ("x: %s, y: %s" % tuple) should be === "x: x, y: y"
  }
}