package starling.local


import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._


class LocalsTest extends TestNGSuite {
  @Test
  def test {
    val old = Locals.currentLocals
    Locals.withLocals(Map.empty) {
      assertEquals(Locals.currentLocals, Map.empty)
      Locals.setLocal("some local", Some(1))
      assertEquals(Locals.getLocal("some local"), Some(1))
      Locals.withLocal("some local", 2) {
        assertEquals(Locals.getLocal("some local"), Some(2))
      }
      assertEquals(Locals.currentLocals, Map("some local" -> 1))
    }
    assertEquals(Locals.currentLocals, old)
  }
}