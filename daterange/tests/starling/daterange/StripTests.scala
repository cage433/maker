package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import collection.immutable.TreeMap
import starling.daterange.Day._

class StripTests extends TestNGSuite {
  @Test
  def shouldContainRangesBetweenStartAndEnd {
    val strip = Strip(Month(2010, 1), Month(2010, 4))
    assertEquals(strip.toList.map(_.period),
      List(Month(2010, 1), Month(2010, 2), Month(2010, 3), Month(2010, 4)))
  }

  @Test
  def shouldHaveStripsOfSpreads {
    val strip = Strip(Spread(Month(2010, 1), Month(2010, 2)), Spread(Month(2010, 3), Month(2010, 4)))
    assertEquals(strip.toList.map(s => Spread(s.front, s.back)),
      List(Spread(Month(2010, 1), Month(2010, 2)), Spread(Month(2010, 2), Month(2010, 3)),
        Spread(Month(2010, 3), Month(2010, 4)))
      )
  }

  @Test
  def shouldRequireThatFirstNotBeAfterLast {
    assertTrue(try {
      val x = Strip(Month(2010, 3), Month(2010, 1))
      false
    } catch {
      case _:AssertionError => true
    })
  }

  @Test
  def shouldReturnTheCorrectSize {
    assertEquals(Strip(Month(2010, 1), Month(2010, 4)).size, 4)
  }

  @Test
  def shouldBeOrdered {
    assertTrue(Strip(Month(2010, 1), Month(2010, 2)) < Strip(Month(2010, 2), Month(2010, 3)))
  }

  @Test
  def shouldBeAbleToLookStripsUpInAMap {
    val map = Map(
      Strip(Month(2010, 1), Month(2010, 2)) -> 10,
      Strip(Month(2010, 2), Month(2010, 3)) -> 12,
      Strip(Month(2010, 1), Month(2010, 3)) -> 15)

    assertEquals(map(Strip(Month(2010, 1), Month(2010, 2))), 10)
    assertEquals(map(Strip(Month(2010, 2), Month(2010, 3))), 12)
    assertEquals(map(Strip(Month(2010, 1), Month(2010, 3))), 15)
  }

  @Test
  def shouldBeAbleToLookStripsUpInATreeMap {
    val map = TreeMap(
      Strip(Month(2010, 1), Month(2010, 2)) -> 10,
      Strip(Month(2010, 2), Month(2010, 3)) -> 12,
      Strip(Month(2010, 1), Month(2010, 3)) -> 15)

    assertEquals(map(Strip(Month(2010, 1), Month(2010, 2))), 10)
    assertEquals(map(Strip(Month(2010, 2), Month(2010, 3))), 12)
    assertEquals(map(Strip(Month(2010, 1), Month(2010, 3))), 15)
  }

  @Test
  def shouldParseStrings {
    assertEquals(Strip.parse("jan11- feb-11"), Some(Strip(Month(2011, 1), Month(2011, 2))))
    assertEquals(Strip.parse("jan11 to feb11"), Some(Strip(Month(2011, 1), Month(2011, 2))))
    assertEquals(Strip.parse("jan 11 to feb11"), Some(Strip(Month(2011, 1), Month(2011, 2))))

    assertEquals(Strip.parse("u1 - v1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("u1 -v1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("u1-v1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("u1 ->v1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("u1 -> v1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("u1tov1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("u1 tov1"), Some(Strip(Month(2011, 9), Month(2011, 10))))
    assertEquals(Strip.parse("q410-q311"), Some(Strip(Quarter(2010, 4), Quarter(2011, 3))))


    assertEquals(Strip.parse("q1-11"), None)
    assertEquals(Strip.parse("04-11"), None)
    assertEquals(Strip.parse("04-2011"), None)
    assertEquals(Strip.parse("2004-2011"), Some(Strip(Year(2004), Year(2011))))
    assertEquals(Strip.parse("q1-12"), None)
    assertEquals(Strip.parse("cal-11"), None)
    assertEquals(Strip.parse("u1/v1"), None)
  }

  @Test
  def shouldntParseDateRange {
    assertEquals(DateRange.parse("01Jan2010 - 30Jan2010"), SimpleDateRange(1 Jan 2010, 30 Jan 2010))
    assertEquals(Strip.parse("01Jan2010 - 30Jan2010"), None)
  }
}