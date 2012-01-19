package starling.local


import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations._
import scala.concurrent.ops._

class LocalsTest extends TestNGSuite {
  @Test
  def simple {
    val old = Locals.currentLocals
    Locals.newStack {
      assertEquals(Locals.currentLocals, Map.empty)
      Locals.withLocal("some local", "1") {
        val local = Locals.getLocal("some local")
        assertEquals(local, Some("1"))
        Locals.withLocal("some local", "2") {
          assertEquals(Locals.getLocal("some local"), Some("2"))
        }
        assertEquals(Locals.currentLocals, Map("some local" -> Local("1", None)))
      }
    }
    assertEquals(Locals.currentLocals, old)
  }

  @Test
  def accessed {
    val old = Locals.currentLocals
    Locals.newStack {
      val (res, rec) = Locals.recordAccessedLocals {
        Locals.withStaticLocal("some local 1", Local("1", Some(1))) {
          val local = Locals.getLocal("some local 1")
          assertEquals(local, Some("1"))
          val f = future {
            Locals.withLocal("some local 2", "2") {
              Locals.withLocal("some local 3", "3") {
                assertEquals(Locals.getLocal("some local 1"), Some("1"))
                assertEquals(Locals.peekLocal("some local 2"), Some("2"))
                assertEquals(Locals.getLocal("some local 3"), Some("3"))
              }
            }
          }
          f()
        }
      }
      assertEquals(rec, Set(Accessed("some local 1", Some(1)), Accessed("some local 3", None)))
    }
    assertEquals(Locals.currentLocals, old)
  }

  @Test(expectedExceptions = Array(classOf[IllegalStateException]))
  def noStackingStaticLocals {
    val old = Locals.currentLocals
    Locals.newStack {
      Locals.withStaticLocal("some local", Local("1", Some(1))) {
        val local = Locals.getLocal("some local")
        assertEquals(local, Some("1"))
        Locals.withStaticLocal("some local", Local("1", Some(2))) {
          assertTrue(false) // shouldn't get here
        }
        assertEquals(Locals.currentLocals, Map("some local" -> Local("1", None)))
      }
    }
    assertEquals(Locals.currentLocals, old)
  }
}