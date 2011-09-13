package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.scalatest.matchers.ShouldMatchers
import collection.mutable.{HashMap, ListBuffer}
import java.util.concurrent.CountDownLatch
import org.scalatest.Ignore

class LogTest extends TestNGSuite with ShouldMatchers {
  import Levels._

  @Test def moreInclusiveLoggingLevelsShouldBeGreaterThanLessInclusiveLevels { // a ⊃ b <=> a > b
    val levels = List(All, Trace, Debug, Info, Warn, Error, Fatal, Off)

    val invalidLevels = (levels zip (levels.tail)).filterNot { case (moreInclusive, lessInclusive) =>
      moreInclusive > lessInclusive
    }

    invalidLevels should be === Nil
  }

  @Test def shouldBeAbleToTemporarilyReduceLoggingLevel {
    val currentLevel = Log.level

    Levels.values.filter(_ <= currentLevel).forall(level => Log.level(level) { Log.level } == level) should be === true

    if (currentLevel >= Info) {
      val levels = Log.level(Info) {
        Log.level :: Log.level(Warn) { Log.level } :: Nil
      }

      levels should be === List(Info, Warn)
    }
  }

  // If it's the case that we'll only change the logging level in tests then increasing is fine.
  @Test(enabled = false) def cannotIncreaseLoggingLevel {
    val currentLevel = Log.level

    if (currentLevel >= Info) {
      Log.level(All) { Log.level } should be === currentLevel
    }
  }

  @Test def temporarilyChangingLoggingLevelOnlyAffectsCurrentThreadAndThreadsSpawnedByTheCurrentThread {
    val levels = new HashMap[String, Levels.Value]

    levels("outer.level") = Log.level

    val ensureSiblingRunningWhenThreadHasChangedLoggingLevel = new CountDownLatch(1)
    val siblingDone = new CountDownLatch(1)

    val thread = new Thread {
      override def run() {
        Log.level(Warn) {
          ensureSiblingRunningWhenThreadHasChangedLoggingLevel.countDown
          siblingDone.await

          levels("thread.level") = Log.level

          val innerThreadShouldHaveItsLoggingLevelChanged = new Thread {
            override def run() {
              levels("innerthread.level") = Log.level
            }
          }

          innerThreadShouldHaveItsLoggingLevelChanged.start
          innerThreadShouldHaveItsLoggingLevelChanged.join
        }
      }
    }

    val siblingThreadShouldNotBeAffectedByTheChangedLoggingLevel = new Thread {
      override def run() {
        ensureSiblingRunningWhenThreadHasChangedLoggingLevel.await

        levels("siblingthread.level") = Log.level

        siblingDone.countDown
      }
    }

    thread.start; siblingThreadShouldNotBeAffectedByTheChangedLoggingLevel.start
    thread.join; siblingThreadShouldNotBeAffectedByTheChangedLoggingLevel.join

    levels("thread.level") should be === Warn
    levels("siblingthread.level") should be === levels("outer.level")
    levels("innerthread.level") should be === levels("thread.level")
  }

  @Test def disablingLoggingWorks {
    Log.off { Log.level } should be === Off
  }

  // Have to think of another way to run performance tests
  @Test(timeOut = 100L, enabled = false) def testDynamicVariableUsageDoesNotSlowLoggingDown = Log.off {
    Log.level should be === Off

    Range(0, 100000).foreach { _ =>
      Log.debug("if you can see this in a log, something is wrong") // 1 million (inactive) logs / s should be enough for anybody
    }
  }
}