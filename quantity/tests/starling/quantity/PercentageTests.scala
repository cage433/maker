package starling.quantity

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._

class PercentageTests extends TestNGSuite {
  @Test
  def testAlmostEquals {
    val p1 = Percentage(.03123456)
    val p2 = Percentage(.031234567)
    assertEquals(p1, p2)
    val p3 = Percentage(.03123455)
    assertNotSame(p1, p3)
  }

  @Test
  def testHashCode {
    val p1 = Percentage(.03123456)
    val p2 = Percentage(.031234567)
    assertEquals(p1.hashCode, p2.hashCode)
    val p3 = Percentage(.03123455)
    assertEquals(p1.hashCode, p3.hashCode)
    val p4 = Percentage(.032)
    assertNotSame(p1.hashCode, p4.hashCode)
  }

  @Test
  def testToString {
    assertEquals(Percentage(.3).toString, "30.0%")
  }
}
