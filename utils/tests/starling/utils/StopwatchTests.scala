package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._


class StopwatchTests extends TestNGSuite with ShouldMatchers {
  @Test
  def testToString {

    Assert.assertEquals(Stopwatch.milliToHumanString(1), "1(ms)")
    Assert.assertEquals(Stopwatch.milliToHumanString(1001), "1(s) 1(ms)")
    Assert.assertEquals(Stopwatch.milliToHumanString((2*60*1000) + (15*1000) + 34), "2(m) 15(s)")
  }

  @Test def testOffsettingToPastIncreasesTimeElapsedAccordingly {
    frozenStopWatch |> (stopWatch => stopWatch.nanos should be === stopWatch.offset(-60).nanos - 60000000)
  }

  @Test def testOffsettingToFutureDecreasesTimeElapsedAccordingly {
    frozenStopWatch |> (stopWatch => stopWatch.nanos should be === stopWatch.offset(60).nanos + 60000000)
  }

  private def frozenStopWatch = new Stopwatch("frozen") {
    override val currentTime = super.currentTime()
  }
}
