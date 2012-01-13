package starling.concurrent

import org.testng.Assert._
import starling.concurrent.MP._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.{AfterTest, Test}
import starling.local.Locals

class MPTests extends TestNGSuite {

  @Test
  def testSimple1 {
    val work = 1 to 10
    val resMP = work.mpMap {
      i => "test" + i
    }.toList
    val res = work.map {
      i => "test" + i
    }.toList
    assertEquals(resMP, res)
  }

  @Test
  def testSimple3 {
    val work = 1 to 10
    val resMP = work.mpMap {
      i => "test" + i
    }.toList
    val res = work.map {
      i => "test" + i
    }.toList
    assertEquals(resMP, res)
  }

  @Test
  def testSimple2 {
    val work = 1 to 10
    val resMP = work.mpMap {
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    val res = work.map {
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    assertEquals(resMP, res)
  }

  @Test
  def testSimpleFlat {
    val work = 1 to 10
    val resMP = work.mpFlatMap {
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    val res = work.flatMap {
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    assertEquals(resMP, res)
  }

  @Test(expectedExceptions = Array(classOf[Exception]))
  def testException {
    val work = 1 to 10
    val res = work.mpFlatMap {
      case i if i == 5 => throw new Exception("test")
      case i => "test" + i
    }.toList
  }

  @Test
  def testInheritingLocals {
    val list = List(1, 2, 3)
    Locals.withLocals(Map.empty) {
      Locals.setLocal("one", Some(1))
      Locals.withLocal("two", 2) {
        list.mpMap {
          i => {
            assertEquals(Locals.getLocal("one"), Some(1))
            assertEquals(Locals.getLocal("two"), Some(2))
          }
        }
      }
    }
  }
}