package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import scala.util.Random

class HalfYearTests extends TestNGSuite {
  @Test
  def testHalfYearFront {
    val halfYear = new HalfYear(2010, FrontOrBack.Front)

    assertEquals(halfYear.firstDay, Day(2010, 1, 1))
    assertEquals(halfYear.lastDay, Day(2010, 6, 30))
  }

  @Test
  def testHalfYearBack {
    val halfYear = new HalfYear(2010, FrontOrBack.Back)

    assertEquals(halfYear.firstDay, Day(2010, 7, 1))
    assertEquals(halfYear.lastDay, Day(2010, 12, 31))
  }

  @Test
  def testContaining {
    val range = new SimpleDateRange(Day(2009, 6, 1), Day(2011, 5, 30))
    range.foreach((day) => {
      val halfYear = HalfYear.containing(day)
      assertTrue(halfYear.firstDay <= day, "half-year " + halfYear + " lower bound doesn't contain " + day)
      assertTrue(halfYear.lastDay >= day, "half-year " + halfYear + " upper bound doesn't contain " + day)
    })
  }

  @Test
  def testToString {
    val halfYear = new HalfYear(2010, FrontOrBack.Back)
    assertEquals(halfYear.toString, "Y2010 BACK")
  }

  @Test
  def testArithmetic {
    val random = new Random(123456)

    assertEquals(new HalfYear(2010, FrontOrBack.Front) + 1, new HalfYear(2010, FrontOrBack.Back))
    assertEquals(new HalfYear(2010, FrontOrBack.Back) + 1, new HalfYear(2011, FrontOrBack.Front))
    assertEquals(new HalfYear(2010, FrontOrBack.Front) + 4, new HalfYear(2012, FrontOrBack.Front))

    (0 to 100).foreach((j) => {
      val randomDay = Day(2010, 1, 1) + random.nextInt(365 * 10)
      val halfYear = HalfYear.containing(randomDay)

      (0 to 10).foreach((i) => {
        val advanced = halfYear + i
        assertEquals(advanced - halfYear, i, "" + advanced + " - " + halfYear + " != " + i)
      })
    })
  }

  @Test
  def shouldParseLikeYearWithHalfAppended {
    assertEquals(HalfYear.parse("10 back"), new HalfYear(2010, FrontOrBack.Back))
    assertEquals(HalfYear.parse("89 front"), new HalfYear(1989, FrontOrBack.Front))
    assertEquals(HalfYear.parse("2010 BACK"), new HalfYear(2010, FrontOrBack.Back))
  }

  @Test
  def shouldParseShorthand {
    assertEquals(HalfYear.parse("2h10"), new HalfYear(2010, FrontOrBack.Back))
    assertEquals(HalfYear.parse("2h 10"), new HalfYear(2010, FrontOrBack.Back))
    assertEquals(HalfYear.parse("2h-10"), new HalfYear(2010, FrontOrBack.Back))
    assertEquals(HalfYear.parse("2h0"), new HalfYear(2010, FrontOrBack.Back))
    assertEquals(HalfYear.parse("2h-0"), new HalfYear(2010, FrontOrBack.Back))

    assertEquals(HalfYear.parse("h2-0"), new HalfYear(2010, FrontOrBack.Back))
    assertEquals(HalfYear.parse("h2-10"), new HalfYear(2010, FrontOrBack.Back))

    assertEquals(HalfYear.parse("1h10"), new HalfYear(2010, FrontOrBack.Front))
    assertEquals(HalfYear.parse("1h 10"), new HalfYear(2010, FrontOrBack.Front))
    assertEquals(HalfYear.parse("1h-10"), new HalfYear(2010, FrontOrBack.Front))
    assertEquals(HalfYear.parse("1h0"), new HalfYear(2010, FrontOrBack.Front))
  }
}