package starling.quantity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test


class ConversionsTests extends TestNGSuite with ShouldMatchers {
  @Test
  def shouldContainInverses {
    val inverseConversions: Iterable[(UOM, Option[Double], Option[Double])] =
      Conversions.fixed_conversions.map {case (uom, conversion) =>
        (uom, Some(1 / conversion), Conversions.default.convert(uom.numeratorUOM, uom.denominatorUOM))
      }

    inverseConversions.foreach{case(uom, expectedInverse, actualInverse) => {
      println(uom + ": " + actualInverse)
      actualInverse should not be ===(Some(0.0))
      actualInverse should be === expectedInverse
    }}
  }
}