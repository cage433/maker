package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import Day._

class MonthTests extends TestNGSuite {
  @Test
  def testArithmetic {
    assertEquals(Month(2010,  2) + 1, Month(2010,  3))
    assertEquals(Month(2010, 12) + 1, Month(2011,  1))
    assertEquals(Month(2010,  2) - 1, Month(2010,  1))
    assertEquals(Month(2010,  1) - 1, Month(2009, 12))
    assertEquals(Month(2010,  1) - 13, Month(2008, 12))
    assertEquals(Month(2010,  1) + 12, Month(2011, 1))
    assertEquals(Month(2010,  1) - 11, Month(2009, 2))
    assertEquals(Month(2010,  1) - 12, Month(2009, 1))
  }

  @Test
  def testArithmetic2{
    // Inefficient but intuitive 
    def plusMonths(m : Month, n : Int) : Month = {
      if (n == 0)
        m
      else
        plusMonths(m.next, n - 1)
    }
    val months = (0 to 50).toArray.map(plusMonths(Month(2010, 1), _))
    for (i <- 0 to 50;
        j <- 0 to 50)
    {
      assertEquals(months(i) + (j - i), months(j))
    }
  }

  @Test
  def testParse {
    val jan10 = Month(2010, 1)
    assertEquals(Month.parse("Jan-2010"), jan10)
    assertEquals(Month.parse("Jan 2010"), jan10)
    assertEquals(Month.parse("Jan 10"), jan10)
    assertEquals(Month.parse("January 10"), jan10)
    assertEquals(Month.parse("January 2010"), jan10)
    assertEquals(Month.parse("F0"), jan10)
    assertEquals(Month.parse("F10"), jan10)
    assertEquals(Month.parse("F2010"), jan10)
    assertEquals(Month.parse("01/10"), jan10)
    assertEquals(Month.parse("01/2010"), jan10)
  }

  @Test
  def testThirdWednesday {
    assertEquals(Month(2010, 6).thirdWednesday, 16 Jun 2010)
    assertEquals(Month(2012, 1).thirdWednesday, 18 Jan 2012)
  }

  @Test
  def testFirstTuesday {
    assertEquals(Month(2010, 6).firstTuesday, 1 Jun 2010)
    assertEquals(Month(2011, 1).firstTuesday, 4 Jan 2011)
    assertEquals(Month(2012, 1).firstTuesday, 3 Jan 2012)
  }
}
