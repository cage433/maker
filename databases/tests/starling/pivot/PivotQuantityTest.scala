package starling.pivot

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.{Percentage, Quantity, UOM}
import org.scalatest.matchers.ShouldMatchers
import starling.utils.ImplicitConversions._
import starling.marketdata.PriceData
import starling.daterange.Month

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

  @Test
  def equalityUsesFormattedValues {
    val lower = PivotQuantity(Quantity(10.0000, UOM.GBP))
    val higher = PivotQuantity(Quantity(10.00004, UOM.GBP))
    val highest = PivotQuantity(Quantity(10.0001, UOM.GBP))

    assertEquals(lower, higher)
    assertEquals(lower.hashCode(), higher.hashCode)

    assertFalse(lower.equals(highest), "%s should not equal %s" % (lower.formattedValues, highest.formattedValues))
    assertFalse(lower.hashCode().equals(highest.hashCode()), "%s should not have the same hashCode as %s" % (lower, highest))
  }

  @Test
  def formattedValuesIncludes4DecimalPlaces {
    val lower = PivotQuantity(Quantity(10.0000, UOM.GBP))
    val higher = PivotQuantity(Quantity(10.00004, UOM.GBP))
    val highest = PivotQuantity(Quantity(10.0001, UOM.GBP))

    assertEquals(lower.formattedValues, Map(UOM.GBP → "10"))
    assertEquals(higher.formattedValues, Map(UOM.GBP → "10"))
    assertEquals(highest.formattedValues, Map(UOM.GBP → "10.0001"))
  }

  @Test
  def priceDataEqualityUsesFormattedPivotQuantities {
    def pq(value: Double): PivotQuantity = PivotQuantity(Map((UOM.USD / UOM.GAL) → value), Map(), None)

    val expected = PriceData(Map(
      Month(2011, 3) → pq(250.52500000000003),
      Month(2011, 4) → pq(263.20000000000005),
      Month(2011, 5) → pq(264.36500000000007),
      Month(2011, 6) → pq(264.76000000000005),
      Month(2011, 7) → pq(264.64500000000004),
      Month(2011, 8) → pq(263.82000000000005),
      Month(2011, 9) → pq(262.12000000000006),
      Month(2011, 10) → pq(251.85500000000008),
      Month(2011, 11) → pq(250.27000000000007),
      Month(2011, 12) → pq(249.65500000000006),
      Month(2012, 1) → pq(249.62000000000006),
      Month(2012, 2) → pq(249.62000000000006)
    ))

    val actual = PriceData(Map(
      Month(2011, 3) → pq(250.525),
      Month(2011, 4) → pq(263.2),
      Month(2011, 5) → pq(264.365),
      Month(2011, 6) → pq(264.76),
      Month(2011, 7) → pq(264.645),
      Month(2011, 8) → pq(263.82),
      Month(2011, 9) → pq(262.12),
      Month(2011, 10) → pq(251.855),
      Month(2011, 11) → pq(250.27),
      Month(2011, 12) → pq(249.655),
      Month(2012, 1) → pq(249.62),
      Month(2012, 2) → pq(249.62)
    ))

    assertEquals(actual, expected)
  }
}
