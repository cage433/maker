package starling.quantity.utils

import org.testng.Assert._
import starling.quantity.{Quantity, UOM}
import starling.pivot.PivotQuantity

object QuantityTestUtils{
  /** Assert that UOMs match and that values are within tolerance
   */
  def assertQtyEquals(actual : Quantity, expected : Quantity, tol : Double = 0.0, message : String = ""){
    if(!actual.isAlmostEqual(expected, tol)) {
      throw new java.lang.AssertionError("assertion failed: " + message + "(" + actual.toStringAllDecimalPlaces() + " != " + expected.toStringAllDecimalPlaces() + " )")
    }
  }

  def assertPivotQtyEquals(actualPQ : PivotQuantity, expected : Quantity, tol : Double = 0.0, message : String = ""){
    val actual = actualPQ.quantityValue.get
    if(!actual.isAlmostEqual(expected, tol)) {
      throw new java.lang.AssertionError("assertion failed: " + message + "(" + actual + " != " + expected + " )")
    }
  }

  def assertQtyOptionClose(actual : Option[Quantity], expected : Option[Quantity], tol : Double = 1e-3, min : Double = 1e-9, message : String = ""){
    assert(actual.isDefined, "Actual is None")
    assert(expected.isDefined, "Expected is None")
    assertQtyClose(actual get, expected get, tol, min, message)
  }


  /** Assert that the percentage difference between two quantities is small
   */
  def assertQtyClose(actual : Quantity, expected : Quantity, tol : Double = 1e-8, min : Double = 1e-9, message : String = ""){
    val actualBase = actual.inBaseUOM
    val expectedBase = expected.inBaseUOM
    assertEquals(actualBase.uom, expectedBase.uom, message)
    val (a, e) = (actualBase.value, expectedBase.value)
    if (a.abs < min){
      // Dealing with very small values,
      assert(a * e >= 0, "Signs differ " + message)
      assert(e.abs < 2 * min, "More than double " + message)
    } else {
      val diff = (a - e) / a
      assert(diff.abs <= tol, "Expected " + expectedBase + ", got " + actualBase + "\ndiff = "+ diff + "\n" +  message)
    }
  }
}
