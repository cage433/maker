package starling.daterange

import starling.daterange.Day._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.utils.ImplicitConversions._

class TenorTest extends TestNGSuite {
  @Test
  def testParse {
    import Tenor._

    assertEquals(TenorType.parseTenor("02Aug2011"), Day(2011, 8, 2))
    assertEquals(TenorType.parseTenor("August 2011"), Month(2011, 8))
    assertEquals(TenorType.parseTenor("2011"), Year(2011))
    assertEquals(TenorType.parseTenor("40544.0"), Day(2011, 1, 1))

    assertEquals(
      List("ON", "SN", "CASH", "CURMON", "CURQ", "1D", "1Y", "1M", "5Q", "8MON").map(Tenor.parse),
      List(ON, SN, CASH, Tenor(Month, 0), Tenor(Quarter, 0), OneDay, OneYear, OneMonth, Tenor(Quarter, 5), Tenor(Month, 8))
    )

    assertEquals("4Aug2011" partialMatch { case Tenor.Parse(tenor) => tenor }, None)
  }

  @Test
  def shouldCalculateIntersectingPeriods {
    assertEquals(Month.intersectingPeriods(Quarter(2010, 2)),
      List(Month(2010, 4), Month(2010, 5), Month(2010, 6)))

    assertEquals(Quarter.intersectingPeriods(Quarter(2010, 2)),
      List(Quarter(2010, 2)))

    assertEquals(Year.intersectingPeriods(Quarter(2010, 2)), 
      List(Year(2010)))

    assertEquals(Week.intersectingPeriods(Month(2010, 4)),
      List(Week(2010, 13), Week(2010, 14), Week(2010, 15), Week(2010, 16), Week(2010, 17)))

    assertEquals(Month.intersectingPeriods(Week(2010, 13)), 
      List(Month(2010, 3), Month(2010, 4)))
  }

  @Test
  def testParseInterval{
    assertEquals(
      TenorType.parseInterval("1M"),
      (1, Month)
    )
    assertEquals(
      TenorType.parseInterval("12Y"),
      (12, Year)
    )
    assertEquals(
      TenorType.parseInterval("9D"),
      (9, Day)
    )
  }
}
