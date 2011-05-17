package starling.utils


import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

import MemoizedFunction._
import org.testng.Assert._

class MemoizedFunctionTests extends TestNGSuite {
  @Test
  def test{
    def fn (x : String) : Int = x.length
    val memFn = memoize(fn)
    assertEquals(memFn.size, 0)
    assertEquals(memFn("12345"), 5)
    assertEquals(memFn.size, 1)
    assertEquals(memFn("abc"), 3)
    assertEquals(memFn.size, 2)
  }

  @Test
  def testWithTwoArgs{
    def fn (x : String, y : String) : String = x + y
    val memFn = memoize2(fn)
    assertEquals(memFn.size, 0)
    assertEquals(memFn("12345", "abc"), "12345abc")
    assertEquals(memFn.size, 1)
  }
}
