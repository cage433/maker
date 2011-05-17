package starling.calendar

import org.testng.annotations.Test
import org.scalatest.testng.TestNGSuite
import org.testng.Assert._

class BrentMonthTest extends TestNGSuite {

  @Test
  def testToString {
    assertEquals(new BrentMonth(2).toString, "Brent FEBRUARY")
    assertEquals(new BrentMonth(12).toString, "Brent DECEMBER")
  }

  @Test
  def testUnapply {
    val feb = new BrentMonth(2)
    assertEquals(Some(feb), BrentMonth.parse(feb.toString))
  }
}