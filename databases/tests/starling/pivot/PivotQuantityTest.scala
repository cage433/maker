package starling.pivot

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.{Percentage, Quantity, UOM}
import org.scalatest.matchers.ShouldMatchers

/**
 */

class PivotQuantityTest extends StarlingTest with ShouldMatchers {
  
  @Test
  def testAdditionMatchingUnits {
    val a = PivotQuantity(Quantity(50, UOM.USD))
    val b = PivotQuantity(Quantity(20, UOM.USD))
    val expected = PivotQuantity(Quantity(50+20, UOM.USD))
    assertEquals(a + b, expected)
  }

  @Test
  def testAdditionDifferentUnits {
    val a = PivotQuantity(Quantity(50, UOM.USD))
    val b = PivotQuantity(Quantity(20, UOM.GBP))
    val expected = new PivotQuantity(Map(UOM.USD -> 50.0, UOM.GBP -> 20.0), Map[String,List[StackTrace]]())
    assertEquals(a + b, expected)
  }

  @Test
  def testDifferenceSameUnits {
    val a = PivotQuantity(Quantity(50, UOM.USD))
    val b = PivotQuantity(Quantity(20, UOM.USD))
    val expected = PivotQuantity(Quantity(30, UOM.USD))
    assertEquals(a - b, expected)
  }

  @Test
  def testDifferenceDifferentUnits {
    val a = PivotQuantity(Quantity(50, UOM.USD))
    val b = PivotQuantity(Quantity(20, UOM.GBP))
    val expected = PivotQuantity(Quantity(50, UOM.USD)) + PivotQuantity(Quantity(-20, UOM.GBP))
    assertEquals(a - b, expected)
  }

  @Test
  def testDifferenceWithErrors {
    val e = new Exception
    val a = PivotQuantity(Quantity(50, UOM.GBP)) + new PivotQuantity(e)
    val b = PivotQuantity(Quantity(50, UOM.GBP))
    val expected = PivotQuantity(Quantity(0, UOM.GBP))  + new PivotQuantity(e)
    assertEquals(a - b, expected)
  }

  @Test
  def testPercentageDifferenceMatchingUnits {
    val a = PivotQuantity(Quantity(100, UOM.USD))
    val b = PivotQuantity(Quantity(110, UOM.USD))
    val expected = Some(Percentage( (110.0-100.0)/100.0 ))
    assertEquals(a.percentageDifference(b), expected)
  }

  @Test
  def testPercentageDifferenceExactMatch {
    val a = PivotQuantity(Quantity(50, UOM.USD))
    val expected = Some(Percentage(0))
    assertEquals(a.percentageDifference(a), expected)
  }

  @Test
  def testPercentageDifferenceWithZero {
    val a = PivotQuantity(Quantity(0, UOM.USD))
    val zero = PivotQuantity(Quantity(20, UOM.USD))
    assertEquals(zero.percentageDifference(a), None)
    assertEquals(a.percentageDifference(zero), None)
    assertEquals(zero.percentageDifference(zero), Some(Percentage(0)))
  }

  @Test
  def testPercentageDifferenceWithManyUnits {
    val a = PivotQuantity(Quantity(10, UOM.USD)) + PivotQuantity(Quantity(10, UOM.GBP))
    assertEquals(a.percentageDifference(a), None)
  }

  @Test
  def testPercentageDifferenceWithError {
    val a = PivotQuantity(Quantity(10, UOM.GBP))
    val b = PivotQuantity(Quantity(10, UOM.USD)) + new PivotQuantity(new Exception)
    assertEquals(a.percentageDifference(b), None)
  }

  @Test
  def testExceptionsWithTheSameMessageButDifferentStackTracesCountAsOneErrorMessage {
    val exception1 = new Exception("<Message>")
    val exception2 = {
      def foo() = { new Exception("<Message>")}
      foo()
    }
    val pq = new PivotQuantity(Set[Throwable](exception1, exception2))
    assertEquals(pq.errors.size, 1)
  }

  @Test
  def testNull{
    assertEquals(new PivotQuantity(Map[UOM, Double](), Map[String, List[StackTrace]]()),
                 PivotQuantity(Quantity.NULL).filterNulls)
  }

  @Test
  def shouldUseLotsFormatWhenUOMHasScaleOf1000 {
    val dp = PivotFormatter.DefaultDecimalPlaces
    assertEquals(dp.format(UOM.K_BBL), PivotFormatter.LotsFormat)
    assertEquals(dp.format(UOM.BBL * 1000), PivotFormatter.LotsFormat)
    assertEquals(dp.format(UOM.USD * 1000), PivotFormatter.LotsFormat)
  }

  @Test
  def explanationShouldReturnQuantityText {
    PivotQuantity(Quantity(10, UOM.USD)).explanation should be === Some(Quantity(10, UOM.USD).toString)
  }

  @Test
  def explanationShouldReturnNamedQuantityExpression {
    val named = Quantity(10, UOM.USD).named("abc")
    PivotQuantity(named).explanation should be === Some(named.format(1))
  }
}
