package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import scala.util.Random

class QuarterTests extends TestNGSuite {
  @Test
  def testQuarterIsThreeMonths {
    val quarter = new Quarter(2010, 2)
    val months = List(new Month(2010, 4), new Month(2010, 5), new Month(2010, 6))

    assertEquals(quarter.days.toList, (List[Day]() /: months)(_ ::: _.days.toList))
  }

  @Test
  def testContaining {
    val range = new SimpleDateRange(Day(2009, 6, 1), Day(2011, 5, 30))
    range.foreach((day) => {
      val quarter = Quarter.containing(day)
      assertTrue(quarter.firstDay <= day, "quarter " + quarter + " lower bound doesn't contain " + day)
      assertTrue(quarter.lastDay >= day, "quarter " + quarter + " upper bound doesn't contain " + day)
    })
  }

  @Test
  def testToString {
    val quarter = new Quarter(2010, 2)
    assertEquals(quarter.toString, "2010Q2")
  }


  @Test
  def testArithmetic {
    val random = new Random(123456)

    assertEquals(new Quarter(2010, 1) + 1, new Quarter(2010, 2))
    assertEquals(new Quarter(2010, 1) + 4, new Quarter(2011, 1))

    (0 to 100).foreach((j) => {
      val randomDay = Day(2010, 1, 1) + random.nextInt(365 * 10)
      val quarter = Quarter.containing(randomDay)

      (0 to 10).foreach((i) => {
        val advanced = quarter + i
        assertEquals(advanced - quarter, i, "" + advanced + " - " + quarter + " != " + i)
      })
    })
  }

  @Test
  def shouldParseWithQAtTheBeginningAndTwoDigitYear {
    assertEquals(Quarter.parse("Q109"), Quarter(2009, 1))
    assertEquals(Quarter.parse("q489"), Quarter(1989, 4))
    assertEquals(Quarter.parse("q4-89"), Quarter(1989, 4))
    assertEquals(Quarter.parse("q4 89"), Quarter(1989, 4))
    assertEquals(Quarter.parse("Q111"), Quarter(2011, 1))
  }

  @Test
  def shouldParseWithQInTheMiddleAndFourDigitYear {
    assertEquals(Quarter.parse("2009Q1"), Quarter(2009, 1))
    assertEquals(Quarter.parse("1989q4"), Quarter(1989, 4))
    assertEquals(Quarter.parse("2011Q1"), Quarter(2011, 1))
  }
  
  @Test
  def shouldParseWithQInTheMiddleAndTwoDigitYear {
    assertEquals(Quarter.parse("09Q1"), Quarter(2009, 1))
    assertEquals(Quarter.parse("89q4"), Quarter(1989, 4))
    assertEquals(Quarter.parse("11Q1"), Quarter(2011, 1))
  }
}
