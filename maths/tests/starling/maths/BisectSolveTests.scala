package starling.maths

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import math._


class BisectSolveTests extends TestNGSuite {

  @Test
  def testSinEqualsZeroAtZero{
    val x = BisectSolve(sin _, -0.5, 0.2, 1e-6)
    assertTrue(abs(sin(x)) < 1e-6)
  }

  @Test
  def testLog{
    val x = BisectSolve(log _, log(5.0), 4.0, 6.0, 1e-6)
    assertTrue(abs(log(x) - log(5)) < 1e-6)

  }
}
