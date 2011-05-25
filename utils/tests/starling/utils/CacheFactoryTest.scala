package starling.utils

import org.scalatest.testng.TestNGSuite
import cache.CacheFactory
import org.testng.annotations.Test
import org.testng.Assert._
import java.lang.String

class CacheFactoryTest extends TestNGSuite {
  @Test
  def testMemoize {
    val cache = CacheFactory.getCache("testCache1", unique = true)
    var sideEffect = 0
    var res1 = cache.memoize("intkey", {sideEffect += 1; sideEffect})
    var res2 = cache.memoize("intkey", {sideEffect += 1; sideEffect})
    var res3 = cache.memoize("intkey", {sideEffect += 1; sideEffect})
    assertEquals(res1, 1)
    assertEquals(res2, 1)
    assertEquals(sideEffect, 1)
    assertEquals(res3, 1)
  }

  @Test
  def testMemoizeWithKey {
    val cache = CacheFactory.getCache("testCache2", unique = true)
    var sideEffect = "?"
    def m(k: String) = {
      sideEffect += k
      sideEffect
    }
    var res1 = cache.memoize("strkey", m _)
    var res2 = cache.memoize("strkey", m _)
    var res3 = cache.memoize("strkey", m _)
    assertEquals(res1, "?strkey")
    assertEquals(res2, "?strkey")
    assertEquals(sideEffect, "?strkey")
    assertEquals(res3, "?strkey")
  }

  @Test
  def testMemoizeWithTupleKey {
    val cache = CacheFactory.getCache("testCache5", unique = true)
    var sideEffect = "?"
    def m(k: String, k2: Map[String, Int]) = {
      sideEffect += k
      sideEffect
    }
    var res1 = cache.memoize(("strkey", Map[String,Int]()), (tuple : (String, Map[String,Int])) => m(tuple._1, tuple._2))
    var res2 = cache.memoize(("strkey", Map[String,Int]()), (tuple : (String, Map[String,Int])) => m(tuple._1, tuple._2))
    var res3 = cache.memoize(("strkey", Map[String,Int]()), (tuple : (String, Map[String,Int])) => m(tuple._1, tuple._2))
    assertEquals(res1, "?strkey")
    assertEquals(res2, "?strkey")
    assertEquals(sideEffect, "?strkey")
    assertEquals(res3, "?strkey")

  }

  @Test
  def testStats {
    val name1: String = "testCache3"
    val cache1 = CacheFactory.getCache(name1)
    assertEquals(CacheFactory.stats(name1).numHits, 0)
    assertEquals(CacheFactory.stats(name1).numMisses, 0)
    assertEquals(CacheFactory.stats(name1).numInstances, 1)

    cache1.memoize("key", 1 + 1)
    assertEquals(CacheFactory.stats(name1).numHits, 0)
    assertEquals(CacheFactory.stats(name1).numMisses, 1)
    assertEquals(CacheFactory.stats(name1).numInstances, 1)

    cache1.memoize("key", 1 + 1)
    assertEquals(CacheFactory.stats(name1).numHits, 1)
    assertEquals(CacheFactory.stats(name1).numMisses, 1)
    assertEquals(CacheFactory.stats(name1).numInstances, 1)

    cache1.memoize("key2", 1 + 1)
    assertEquals(CacheFactory.stats(name1).numHits, 1)
    assertEquals(CacheFactory.stats(name1).numMisses, 2)
    assertEquals(CacheFactory.stats(name1).numInstances, 1)

    val name2: String = "testCache4"
    val cache2 = CacheFactory.getCache(name2)
    assertEquals(CacheFactory.stats(name2).numHits, 0)
    assertEquals(CacheFactory.stats(name2).numMisses, 0)
    assertEquals(CacheFactory.stats(name2).numInstances, 1)
    cache2.memoize("key", 1 + 1)
    assertEquals(CacheFactory.stats(name2).numHits, 0)
    assertEquals(CacheFactory.stats(name2).numMisses, 1)
    assertEquals(CacheFactory.stats(name2).numInstances, 1)
  }
}
