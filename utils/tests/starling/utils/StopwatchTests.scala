package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._
import org.scalatest.WordSpec


class StopwatchTests extends TestNGSuite with ShouldMatchers {
  @Test
  def testToString {

    Assert.assertEquals(Stopwatch.milliToHumanString(1), "1(ms)")
    Assert.assertEquals(Stopwatch.milliToHumanString(1001), "1(s) 1(ms)")
    Assert.assertEquals(Stopwatch.milliToHumanString((2*60*1000) + (15*1000) + 34), "2(m) 15(s)")
  }

  @Test def testOffsettingToPastIncreasesTimeElapsedAccordingly {
    frozenStopWatch |> (stopWatch => stopWatch.nanos should be === stopWatch.offset(-60).nanos - 60000000000L)
  }

  @Test def testOffsettingToFutureDecreasesTimeElapsedAccordingly {
    frozenStopWatch |> (stopWatch => stopWatch.nanos should be === stopWatch.offset(60).nanos + 60000000000L)
  }

  private def frozenStopWatch = new Stopwatch("frozen") {
    override val currentTime = super.currentTime()
  }
}

class TimingInfoTests extends WordSpec with ShouldMatchers {
  val timingInfo = TimingInfo(0, 100 * 1000000)

  "loggingLevels" should {
    "return debug level when timeElapsed < info threshold" in {
      timingInfo.loggingLevel(Levels.Info → 200) should be === Levels.Debug
      timingInfo.loggingLevel(Map(Levels.Info → 200)) should be === Levels.Debug
      timingInfo.loggingLevel(200) should be === Levels.Debug
    }

    "return info level when timeElapsed >= info threshold" in {
      timingInfo.loggingLevel(Levels.Info → 100) should be === Levels.Info
      timingInfo.loggingLevel(Map(Levels.Info → 100)) should be === Levels.Info
      timingInfo.loggingLevel(100) should be === Levels.Info
    }

    "return warn level when timeElapsed >= warn threshold" in {
      timingInfo.loggingLevel(Levels.Info → 10, Levels.Warn → 100) should be === Levels.Warn
      timingInfo.loggingLevel(Map(Levels.Info → 10, Levels.Warn → 100)) should be === Levels.Warn
      timingInfo.loggingLevel(10) should be === Levels.Warn
    }
  }
}