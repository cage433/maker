package starling.utils

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class MathUtilTests extends WordSpec with ShouldMatchers {
  "Rounding a value with 4dp to 4dp shouldn't lose precision" in {
    MathUtil.roundToNdp(67.2525, 4) should be === 67.2525
  }
}