package starling.models

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.Quantity
import org.testng.Assert._
import starling.maths.{NumericalDerivative, Sample, Lognormal}
import org.scalatest.testng.TestNGSuite

class BlackScholesTests extends TestNGSuite{
  @Test
  def AtTheMoneyBSCallTest = {
    val bs = BlackScholes.undiscountedOptionPrice(100, 100, Call, 1, 0.2)
    assertQtyEquals(bs, 7.965567, 1e-5)
  }

  @Test
  def AtTheMoneyBSPutTest = {
    val bs = BlackScholes.undiscountedOptionPrice(100, 100, Put, 1, 0.2)
    assertQtyEquals(bs, 7.965567, 1e-5)
  }

  @Test
  def InTheMoneyBSCallTest = {
    val bs = BlackScholes.undiscountedOptionPrice(150, 100, Call, 1, 0.2)
    assertQtyEquals(bs, 50.19247, 1e-5)
  }

  @Test
  def OutOfTheMoneyBSPutTest = {
    val bs = BlackScholes.undiscountedOptionPrice(150, 100, Put, 1, 0.2)
    assertQtyEquals(bs, 0.19247, 1e-5)
  }

  @Test
  def OutOfTheMoneyBSCallTest = {
    val bs = BlackScholes.undiscountedOptionPrice(50, 100, Call, 1, 0.2)
    assertQtyEquals(bs, 0.00094, 1e-5)
  }

  @Test
  def InTheMoneyBSPutTest = {
    val bs = BlackScholes.undiscountedOptionPrice(50, 100, Put, 1, 0.2)
    assertQtyEquals(bs, 50.00094, 1e-5)
  }

  @Test
  def intrinsicITMPut = {
    val bs1 = BlackScholes.undiscountedOptionPrice(50, 100, Put, 0, 0.2)
    val bs2 = BlackScholes.undiscountedOptionPrice(50, 100, Put, 1, 0)
    assertQtyEquals(bs1, bs2, 1e-6)
    assertQtyEquals(bs1, 50, 1e-6)
  }

  @Test
  def intrinsicOTMPut = {
    val bs1 = BlackScholes.undiscountedOptionPrice(100, 99, Put, 0, 0.2)
    val bs2 = BlackScholes.undiscountedOptionPrice(100, 99, Put, 1, 0)
    assertQtyEquals(bs1, bs2, 1e-6)
    assertQtyEquals(bs1, 0, 1e-6)
  }

  @Test
  def intrinsicITMCall = {
    val bs1 = BlackScholes.undiscountedOptionPrice(100, 50, Call, 0, 0.2)
    val bs2 = BlackScholes.undiscountedOptionPrice(100, 50, Call, 1, 0)
    assertQtyEquals(bs1, bs2, 1e-6)
    assertQtyEquals(bs1, 50, 1e-6)
  }

  @Test
  def intrinsicOTMCall = {
    val bs1 = BlackScholes.undiscountedOptionPrice(99, 100, Call, 0, 0.2)
    val bs2 = BlackScholes.undiscountedOptionPrice(99, 100, Call, 1, 0)
    assertQtyEquals(bs1, bs2, 1e-6)
    assertQtyEquals(bs1, 0, 1e-6)
  }

  @Test
  def testProbExerciseAndAverageExercisedPrice{
    val K = 100.0
    val T = 0.5
    val vol = 0.3
    val nIterations = 20000

    for (F <- 95.0 to 105.0 by 5.0) {
      for (callPut <- List(Call, Put)) {
        val P = Lognormal(F, vol, T).randomVariable(seed = 12345)
        val samplePriceRatio = Sample(
          () =>
            {val x = P.nextDouble
            if (callPut.isInTheMoney(K, x))
              x / F
            else
              0
            }
          ,
          nIterations
        ).mean
        val sampleProbExercise = Sample({
          () =>
            val x = P.nextDouble
            if (callPut.isInTheMoney(K, x))
              1.0
            else
              0
          },
          nIterations
        ).mean
        val bl = new BlackScholes(F, K, callPut, T, vol)
        assertEquals(sampleProbExercise, bl.probabilityOfExercise, 1e-2)
        assertEquals(samplePriceRatio, bl.expectedPriceFractionGivenExercise, 1e-2)

      }
    }
  }

  @Test
  def testNumericDeltas() {
    val K = 101.0
    for (F <- 95.0 to 105.0 by 5.0;
         callPut <- List(Call, Put);
         vol <- List(0.0, 0.5);
         time <- List(0.0, 1.2))
    {
      val numericDelta = new NumericalDerivative(BlackScholes.undiscountedOptionPrice(_, K, callPut, time, vol), 1e-3)(F)
      val analyticDelta = new BlackScholes(F, K, callPut, time, vol).analyticDelta
      assertEquals(numericDelta, analyticDelta, 1e-5)
    }

  }

@Test
  def testNumericVegas() {
    val K = 101.0
    for (F <- 95.0 to 105.0 by 5.0;
         callPut <- List(Call, Put);
         vol <- 0.3 to 0.5 by 0.05;
         time <- List(0.0, 1.2))
    {
      val numericDelta = new NumericalDerivative(BlackScholes.undiscountedOptionPrice(_, K, callPut, time, vol), 1e-3)(F)
      val numericVega = new NumericalDerivative(BlackScholes.undiscountedOptionPrice(F, K, callPut, time, _), 1e-3)(vol)
      val analyticVega = new BlackScholes(F, K, callPut, time, vol).analyticVega
      assertEquals(numericVega, analyticVega, 1e-4)
    }

  }

  @Test
  def testStrikeFromDelta {
    val F = 100.0
    val K = 150
    val vol = .40
    val T = .25

    val blackScholes = new BlackScholes(F, K, Call, T, vol)
    val delta = blackScholes.analyticDelta
    val strike = blackScholes.strikeFromDelta(delta)

    assertEquals(strike, K, 1e-4)
  }
}
