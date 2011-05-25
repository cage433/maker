package starling.quantity

import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations.Test
import starling.quantity.RichQuantity._
import starling.quantity.UOM._

class RichQuantityTests extends TestNGSuite {
  @Test
  def testThatRichQuantityConvertsToQuantity {
    val richQ = 12.34 (GBP)
    val normalQ = Quantity(12.34, UOM.GBP)

    assertEquals(richQ, normalQ)
  }

  @Test
  def testRichQuantityCanBeUsedInArithmetic {
    assertEquals(1.0 (GBP) + 2.0 (GBP), 3.0 (GBP))
    assertEquals(1.0 (GBP) - 2.0 (GBP), (-1.0) (GBP))
    assertEquals(0.639 (GBP/USD) * 100.0 (USD), 63.9 (GBP))
    assertEquals(1.0 (USD) / 5.0 (GBP), 0.2 (USD/GBP))
  }
}
