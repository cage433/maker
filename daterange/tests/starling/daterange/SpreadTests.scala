package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import collection.immutable.TreeMap

class SpreadTests extends TestNGSuite {
  @Test
  def shouldBeOrdered {
    assertTrue(Spread[Month](Month(2010, 1), Month(2010, 2)) < Spread[Month](Month(2010, 2), Month(2010, 3)))
  }

  @Test
  def shouldBeAbleToLookSpreadsUpInAMap {
    val map = Map(
      Spread[Month](Month(2010, 1), Month(2010, 2)) -> 10,
      Spread[Month](Month(2010, 2), Month(2010, 3)) -> 12,
      Spread[Month](Month(2010, 1), Month(2010, 3)) -> 15)

    assertEquals(map(Spread[Month](Month(2010, 1), Month(2010, 2))), 10)
    assertEquals(map(Spread[Month](Month(2010, 2), Month(2010, 3))), 12)
    assertEquals(map(Spread[Month](Month(2010, 1), Month(2010, 3))), 15)
  }

  @Test
  def shouldBeAbleToLookSpreadsUpInATreeMap {
    val map = TreeMap(
      Spread[Month](Month(2010, 1), Month(2010, 2)) -> 10,
      Spread[Month](Month(2010, 2), Month(2010, 3)) -> 12,
      Spread[Month](Month(2010, 1), Month(2010, 3)) -> 15)

    assertEquals(map(Spread[Month](Month(2010, 1), Month(2010, 2))), 10)
    assertEquals(map(Spread[Month](Month(2010, 2), Month(2010, 3))), 12)
    assertEquals(map(Spread[Month](Month(2010, 1), Month(2010, 3))), 15)
  }

  @Test
  def testNext {
    assertEquals(Spread(Month(2010, 1), Month(2010, 2)).next, Spread(Month(2010, 2), Month(2010, 3)))
    assertEquals(Spread(Month(2010, 11), Month(2010, 12)).next, Spread(Month(2010, 12), Month(2011, 1)))
    assertEquals(Spread(Month(2010, 12), Month(2011, 1)).next, Spread(Month(2011, 1), Month(2011, 2)))
  }

  @Test
  def testParse {
    assertEquals(Spread.parse("g10 / h0"), Some(Spread(Month(2010, 2), Month(2010, 3))))
    assertEquals(Spread.parse("feb10/h0"), Some(Spread(Month(2010, 2), Month(2010, 3))))
    assertEquals(Spread.parse("g0h0"), Some(Spread(Month(2010, 2), Month(2010, 3))))
    assertEquals(Spread.parse("04/11"), None)
    assertEquals(Spread.parse("04/2011"), None)
    assertEquals(Spread.parse("2004/2011"), Some(Spread(Year(2004), Year(2011))))
    assertEquals(Spread.parse("01Mar11-02Apr11/Mar2012"), Some(Spread(DateRange(Day(2011, 3, 1), Day(2011, 4, 2)), Month(2012, 3))))
  }

  @Test
  def testParseAsStrip {
    assertEquals(Spread.parse("g10 / h0"), Some(Spread(Month(2010, 2), Month(2010, 3))))
    assertEquals(Spread.parse("feb10/h0"), Some(Spread(Month(2010, 2), Month(2010, 3))))
    assertEquals(Spread.parse("g0h0"), Some(Spread(Month(2010, 2), Month(2010, 3))))
  }

  @Test
  def testAsStrip {
    assertEquals(Spread.parse("v0/f1"), Some(Spread(Month(2010, 10), Month(2011, 1))))
    assertEquals(Spread.parse("v0/f1").get.iterator.toList,
      List(
        Spread(Month(2010, 10), Month(2010, 11)),Spread(Month(2010, 11), Month(2010, 12)),Spread(Month(2010, 12), Month(2011, 1))
        )
      )
  }
}
