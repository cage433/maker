package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import scala.util.Random

class HalfMonthTests extends TestNGSuite {
  @Test
  def testHalfMonthFront {
    val halfMonth = new HalfMonth(2010, 2, FrontOrBack.Front)

    assertEquals(halfMonth.firstDay, Day(2010, 2, 1))
    assertEquals(halfMonth.lastDay, Day(2010, 2, 15))
  }

  @Test
  def testHalfMonthBack {
    val halfMonth = new HalfMonth(2010, 3, FrontOrBack.Back)
    
    assertEquals(halfMonth.firstDay, Day(2010, 3, 16))
    assertEquals(halfMonth.lastDay, Day(2010, 3, 31))
  }

  @Test
  def testContaining {
    val range = new SimpleDateRange(Day(2009, 6, 1), Day(2011, 5, 30))
    range.foreach((day) => {
      val halfMonth = HalfMonth.containing(day)
      assertTrue(halfMonth.firstDay <= day, "half-month " + halfMonth + " lower bound doesn't contain " + day)
      assertTrue(halfMonth.lastDay >= day, "half-month " + halfMonth + " upper bound doesn't contain " + day)
    })
  }

  @Test
  def testToString {
    val halfMonth = new HalfMonth(2010, 1, FrontOrBack.Back)
    assertEquals(halfMonth.toString, "JANUARY 2010 BACK")
  }

  @Test
  def testArithmetic {
    val random = new Random(123456)

    assertEquals(new HalfMonth(2010, 1, FrontOrBack.Front) + 1, new HalfMonth(2010, 1, FrontOrBack.Back))
    assertEquals(new HalfMonth(2010, 1, FrontOrBack.Back) + 1, new HalfMonth(2010, 2, FrontOrBack.Front))
    assertEquals(new HalfMonth(2010, 1, FrontOrBack.Front) + 24, new HalfMonth(2011, 1, FrontOrBack.Front))
    
    (0 to 100).foreach((j) => {
      val randomDay = Day(2010, 1, 1) + random.nextInt(365 * 10)
      val halfMonth = HalfMonth.containing(randomDay)

      (0 to 10).foreach((i) => {
        val advanced = halfMonth + i
        assertEquals(advanced - halfMonth, i, "" + advanced + " - " + halfMonth + " != " + i)
      })
    })
  }

  @Test
  def shouldParseLikeMonthWithHalfAppended {
    assertEquals(HalfMonth.parse("mar-10 back"),   new HalfMonth(2010,  3, FrontOrBack.Back))
    assertEquals(HalfMonth.parse("dec-89 front"),  new HalfMonth(1989, 12, FrontOrBack.Front))
    assertEquals(HalfMonth.parse("JAN-2010 BACK"), new HalfMonth(2010,  1, FrontOrBack.Back))
  }
}
