package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import java.io.File
import org.testng.Assert._

class PersistentMapTests extends TestNGSuite{

  @Test
  def testUpdatesWorkAsExpected{
    val file = File.createTempFile("PersistentMapTests", "")
    try {
      file.delete
      val map = PersistentMap[String, String](file)
      assertTrue(map.isEmpty)
      assertFalse(file.exists)
      map("foo") = "bar"
      assertEquals(map("foo"), "bar")

      map("fred") = "mike"
      assertEquals(map.size, 2)
      assertEquals(map("fred"), "mike")

      map("fred") = "bill"
      assertEquals(map.size, 2)
      assertEquals(map("fred"), "bill")

      map -= ("fred")
      assertEquals(map.size, 1)
    } finally{
      file.delete()
    }
  }
}

class PersistentSetTests extends TestNGSuite{

  @Test
  def testUpdatesWorkAsExpected{
    val file = File.createTempFile("PersistentSetTests", "")
    try {
      file.delete
      val set = PersistentSet[String](file)
      assertTrue(set.isEmpty)
      assertFalse(file.exists)
      set+= "foo"
      assertTrue(set.contains("foo"))

      set += "fred"
      assertEquals(set.size, 2)
      assertTrue(set.contains("fred"))

      set -= ("fred")
      assertEquals(set.size, 1)
    } finally{
      file.delete()
    }
  }
}