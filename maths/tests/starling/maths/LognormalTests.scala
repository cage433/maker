package starling.maths

import org.scalatest.testng.TestNGSuite
import starling.utils.conversions.RichColtMatrices._
import org.testng.annotations.Test
import math._
import org.testng.Assert._


class LognormalTests extends TestNGSuite {
  @Test
  def testStandardDeviationOfSpread{
    val vol1 = 0.5
    val vol2 = 0.8
    val dT = 1.0
    val F1: Double = 10.0
    val l1 = Lognormal(F1, vol1, dT)
    val F2: Double = 9.0
    val l2 = Lognormal(F2, vol2, dT)
    val rho = 0.5
    val stDev = l1.stDevOfSpread(l2, rho)
    val nSamples = 20000
    val sample = Sample(new LognormalPrices(F1, F2, vol1, vol2, rho, dT).nextValues _, nSamples)

    assertEquals(l1.variance, sample(0).variance, l1.variance * 0.02)
    assertEquals(l2.variance, sample(1).variance, l2.variance * 0.05)
    assertEquals(l1.covariance(l2, rho), sample.covariance(0, 1), l1.covariance(l2, rho) * 0.02)
    val v = sample(0)
    val spreads = sample(0) - sample(1)
    val mcStdDev = spreads.standardDeviation
    assertEquals(stDev, mcStdDev, 0.5)
    
  }
}
