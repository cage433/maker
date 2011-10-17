package starling.daterange

import starling.daterange.Day._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._

class SimpleDateRangeTest extends TestNGSuite {
  @Test
  def testNormalisation {
    assertEquals(SimpleDateRange(Day(2010, 1, 1), Day(2010, 1, 31)).tryToNormalise, Month(2010, 1))
    assertEquals(SimpleDateRange(Day(2010, 1, 1), Day(2010, 12, 31)).tryToNormalise, Year(2010))
    assertEquals(SimpleDateRange(Day(2010, 1, 1), Day(2010, 12, 3)).tryToNormalise, SimpleDateRange(Day(2010, 1, 1), Day(2010, 12, 3)))

    // leap year
    assertEquals(SimpleDateRange(Day(2012, 2, 1), Day(2012, 2, 29)).tryToNormalise, Month(2012, 2))
    assertNotSame(SimpleDateRange(Day(2012, 2, 1), Day(2012, 2, 28)).tryToNormalise, Month(2012, 2))
  }

  @Test
  def testSplitIntoMonths {
    assertEquals(SimpleDateRange(Day(2010, 1, 1), Day(2010, 1, 31)).toListOfMonths, List(Month(2010, 1)))
    assertEquals(SimpleDateRange(Day(2010, 1, 1), Day(2010, 2, 28)).toListOfMonths, List(Month(2010, 1), Month(2010, 2)))
    assertEquals(SimpleDateRange(Day(2010, 3, 1), Day(2010, 5, 31)).toListOfMonths, List(Month(2010, 3),Month(2010, 4), Month(2010, 5)))
    assertEquals(SimpleDateRange(Day(2010, 3, 2), Day(2010, 5, 31)).canSplitIntoMonths, false)
  }

  @Test
  def testParse {
    assertEquals(DateRange.parse("1-10Dec09"), SimpleDateRange(1 Dec 2009, 10 Dec 2009))
    assertEquals(DateRange.parse("1Dec2009-10Dec09"), SimpleDateRange(1 Dec 2009, 10 Dec 2009))
    assertEquals(DateRange.parse("1 -10Dec09"), SimpleDateRange(1 Dec 2009, 10 Dec 2009))
    assertEquals(DateRange.parse("1 -> 10Dec09"), SimpleDateRange(1 Dec 2009, 10 Dec 2009))
    assertEquals(DateRange.parse("1 ->10Dec09"), SimpleDateRange(1 Dec 2009, 10 Dec 2009))
  }
}
