package starling.maths

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import math._


class RandomVariablesTests extends TestNGSuite {
  @Test
  def testNormalDistribution{
    val n = 10000
    val sample = Sample(new StandardNormalVariable(seed = 12345).nextDouble _, n)
    assertEquals(sample.mean, 0.0, 3.0 / sqrt(n))
    assertEquals(sample.stdDev, 1.0, 3.0 / sqrt(n))
  }

  @Test
  def testUncorrelatedNormals{
    val n = 10000

    val sample = Sample(new UncorrelatedStandardNormals(3, seed = 12345).nextValues _, n)
    assertEquals(sample.correlation(0, 1), 0.0, 3.0 / sqrt(n))
    assertEquals(sample.mean(0), 0.0, 3.0 / sqrt(n))
  }

  @Test
  def testCorrelatedNormals {
    val n = 10000
    val rho = 0.5
    val sample = Sample(new CorrelatedStandardNormals(rho, seed = 12345).nextValues _, n)
    assertEquals(sample.correlation(0, 1), rho, 3.0 / sqrt(n))
    assertEquals(sample.mean(0), 0.0, 3.0 / sqrt(n))
    assertEquals(sample.stdDev(1), 1.0, 3.0 / sqrt(n))
  }

}
