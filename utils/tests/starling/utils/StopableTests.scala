package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._


class StopableTests extends TestNGSuite {
  @Test def createWithFunctions {
    var started: Boolean = false
    var stopped: Boolean = false
    val stopable = Stopable(() => started = true, () => stopped = true)

    stopable.start; stopable.stop

    assertEquals(started, true)
    assertEquals(stopped, true)
  }
}