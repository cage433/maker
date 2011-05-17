package starling.models

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.QuantityTestUtils._

class CallOrPutTest extends TestNGSuite {
  @Test
  def testIntrinsicCall {
    assertQtyEquals(Call.intrinsicPrice(100, 50), 50)
    assertQtyEquals(Call.intrinsicPrice(99, 100), 0)
  }

  @Test
  def testIntrinsicPut {
    assertQtyEquals(Put.intrinsicPrice(100, 99), 0)
    assertQtyEquals(Put.intrinsicPrice(50, 100), 50)
  }

@Test
  def testIntrinsicStraddle {
    assertQtyEquals(Straddle.intrinsicPrice(100, 99), 1)
    assertQtyEquals(Straddle.intrinsicPrice(50, 100), 50)
  }
}
