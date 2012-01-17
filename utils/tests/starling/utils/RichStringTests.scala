package starling.utils

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class RichStringTests extends WordSpec with ShouldMatchers with RichString {
  "can replace using map" in {
    "foobar".replaceAll(Map("foo" → "oof", "bar" → "rab")) should be === "oofrab"
  }

  "%" in {
    ("x: %s, y: %s" % ("x", "y")) should be === "x: x, y: y"
    ("x: %s, y: %s" % (("x", "x"), ("y", "y"))) should be === "x: (x,x), y: (y,y)"
  }
}