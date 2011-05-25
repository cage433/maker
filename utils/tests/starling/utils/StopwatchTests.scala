package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert

class StopwatchTests extends TestNGSuite {
  @Test
  def testToString {

    Assert.assertEquals(Stopwatch.milliToHumanString(1), "1(ms)")
    Assert.assertEquals(Stopwatch.milliToHumanString(1001), "1(s) 1(ms)")
    Assert.assertEquals(Stopwatch.milliToHumanString((2*60*1000) + (15*1000) + 34), "2(m) 15(s)")
  }
}
