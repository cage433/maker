package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.daterange.Day
import starling.quantity.Quantity
import starling.utils.ScalaTestUtils._
import org.testng.Assert._

class InterpolationTests extends StarlingTest {

  @Test
  def testInverseConstant{
    val days = Array(Day(2009, 10, 1), Day(2009, 11, 15), Day(2009, 12, 2))
    val prices = Array(0.0, 1.0, 2.0)

   assertEquals(
     InverseConstantInterpolation.interpolate(days, prices, Day(2009, 9, 30)),
     0.0
    )
    assertEquals(
      InverseConstantInterpolation.interpolate(days, prices, Day(2009, 10, 20)),
      0.0
     )
    assertEquals(
      InverseConstantInterpolation.interpolate(days, prices, Day(2009, 11, 15)),
      1.0
     )
    assertEquals(
      InverseConstantInterpolation.interpolate(days, prices, Day(2009, 12, 1)),
      1.0
     )
    assertEquals(
      InverseConstantInterpolation.interpolate(days, prices, Day(2009, 12, 2)),
      2.0
     )
    assertEquals(
      InverseConstantInterpolation.interpolate(days, prices, Day(2009, 12, 20)),
      2.0
     )
  }
  @Test
  def testInverseConstant2{
    val days = Array(Day(2009, 10, 1))
    val prices = Array(1.0)

   assertEquals(
     InverseConstantInterpolation.interpolate(days, prices, Day(2009, 9, 30)),
     1.0
    )
    assertEquals(
      InverseConstantInterpolation.interpolate(days, prices, Day(2009, 10, 20)),
      1.0
     )
  }
}
