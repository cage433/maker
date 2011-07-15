package starling.concurrent

import org.testng.Assert._
import starling.concurrent.MP._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.{AfterTest, Test}

class MPTests extends TestNGSuite {

  @Test
  def testSimple1 {
    val work = 1 to 10
    val resMP = work.mpMap{
      i => "test" + i
    }.toList
    val res = work.map{
      i => "test" + i
    }.toList
    assertEquals(resMP, res)
  }

  @Test
  def testSimple3 {
    val work = 1 to 10
    val resMP = work.mpMap{
      i => "test" + i
    }.toList
    val res = work.map{
      i => "test" + i
    }.toList
    assertEquals(resMP, res)
  }

  @Test
  def testSimple2 {
    val work = 1 to 10
    val resMP = work.mpMap{
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    val res = work.map{
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    assertEquals(resMP, res)
  }

  @Test
  def testSimpleFlat {
    val work = 1 to 10
    val resMP = work.mpFlatMap{
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    val res = work.flatMap{
      case i if i % 2 == 0 => Some(i)
      case i => None
    }.toList
    assertEquals(resMP, res)
  }

  @Test(expectedExceptions = Array(classOf[Exception]))
  def testException {
    val work = 1 to 10
    val res = work.mpFlatMap{
      case i if i == 5 => throw new Exception("test")
      case i => "test" + i
    }.toList
  }
}