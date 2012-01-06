package starling.daterange

import org.testng.annotations.Test
import org.testng.Assert._
import Day._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class MonthTests extends WordSpec with ShouldMatchers {
  "test arithmetic" in {
    Month(2010,  2) + 1 should be === Month(2010,  3)
    Month(2010, 12) + 1 should be === Month(2011,  1)
    Month(2010,  2) - 1 should be === Month(2010,  1)
    Month(2010,  1) - 1 should be === Month(2009, 12)
    Month(2010,  1) - 13 should be === Month(2008, 12)
    Month(2010,  1) + 12 should be === Month(2011, 1)
    Month(2010,  1) - 11 should be === Month(2009, 2)
    Month(2010,  1) - 12 should be === Month(2009, 1)
  }

  "test arithmetic 2" in {
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
      months(i) + (j - i) should be === months(j)
    }
  }

  "test parse" in {
    val jan10 = Month(2010, 1)
    Month.parse("Jan-2010") should be === jan10
    Month.parse("Jan 2010") should be === jan10
    Month.parse("Jan 10") should be === jan10
    Month.parse("January 10") should be === jan10
    Month.parse("January 2010") should be === jan10
    Month.parse("F0") should be === jan10
    Month.parse("F10") should be === jan10
    Month.parse("F2010") should be === jan10
    Month.parse("01/10") should be === jan10
    Month.parse("01/2010") should be === jan10
  }

  "test third wednesday" in {
    Month(2010, 6).thirdWednesday should be === (16 Jun 2010)
    Month(2012, 1).thirdWednesday should be === (18 Jan 2012)
  }

  "test first tuesday" in {
    Month(2010, 6).firstTuesday should be === (1 Jun 2010)
    Month(2011, 1).firstTuesday should be === (4 Jan 2011)
    Month(2012, 1).firstTuesday should be === (3 Jan 2012)
  }

  "test days" in {
    Month(2012, 1).days.groupBy(_.dayOfWeek).foreach {
      case (dayOfWeek, expectedDays) => Month(2012, 1).daysMatching(dayOfWeek) should be === expectedDays.toList
    }
  }
}
