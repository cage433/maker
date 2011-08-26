package starling.daterange

import org.testng.annotations._
import starling.utils.ScalaTestUtils._
import starling.daterange.Day._
import org.testng.Assert._
import org.scalatest.testng.TestNGSuite

class DayAndTimeTests extends TestNGSuite{
  @DataProvider(name = "testTimeSinceProvider")
  def testTimeSinceProvider = constructArgs(
    ((1 Jan 2010).endOfDay, (2 Jan 2010).startOfDay, 0.0),
    ((1 Jan 2010).startOfDay, (1 Jan 2010).endOfDay, 1.0 / daysInYear),
    ((1 Jan 2010).startOfDay, (2 Jan 2010).startOfDay, 1.0 / daysInYear),
    ((1 Jan 2010).startOfDay, (2 Jan 2010).endOfDay, 2.0 / daysInYear)
  )
  @Test(dataProvider = "testTimeSinceProvider")
  def testTimeSince(from : DayAndTime, to : DayAndTime, expected : Double){
    assertEquals(to.timeSince(from), expected, 1e-9)
    assertEquals(from.timeSince(to), -expected, 1e-9)
  }
  @Test
  def testDayFormat {
    assertEquals("15/07/2011", Day(2011, 7, 15).toString("dd/MM/yyyy"))
  }
}
