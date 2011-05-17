package starling.models

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import math._
import starling.maths.NumericalDerivative
import starling.daterange.Day

class FiniteDifferenceTests extends TestNGSuite {

  private def americanCondition(K : Double, callPut : CallOrPut) (price : Double, t : Double, optionPrice : Double) = {
    callPut.intrinsic(K)(price) max optionPrice
  }
  private def europeanCondition (price : Double, t : Double, optionPrice : Double) = {
    optionPrice
  }

  private def makeTimes(nTimes : Int, T : Double) = {
    val dt = T / (nTimes - 1)
    (0 until nTimes).map(_ * dt).toArray
  }


  @DataProvider(name = "makeSeveralOptionsProvider")
  def makeSeveralOptionsProvider = {
    def makeArray(M : Int = 50, exerciseType : ExerciseType, K : Double, callPut : CallOrPut, F : Double, r : Double, vol : Double, nDays : Int) = {
      Array(M, exerciseType, K, callPut, F, r, vol, nDays)
    }
    Array(
      makeArray(K = 100.0, exerciseType = American, callPut = Call, F = 103.0, r = 0.0, vol = 0.5, nDays = 400)
      ,makeArray(K = 100.0, exerciseType = American, callPut = Put, F = 96.0, r = 0.05, vol = 0.5, nDays = 400)
      ,makeArray(K = 400.0, exerciseType = American, callPut = Put, F = 96.0, r = 0.05, vol = 0.05, nDays = 60)
      ,makeArray(K = 100.0, exerciseType = European, callPut = Call, F = 103.0, r = 0.0, vol = 0.5, nDays = 400)
      ,makeArray(K = 100.0, exerciseType = European, callPut = Put, F = 96.0, r = 0.05, vol = 0.5, nDays = 400)
      ,makeArray(K = 400.0, exerciseType = European, callPut = Put, F = 96.0, r = 0.05, vol = 0.05, nDays = 60)
      )
  }

  @Test(dataProvider = "makeSeveralOptionsProvider")
  def testNumbersMatchJonsAlgorithm(
    M : Int, 
    exerciseType : ExerciseType,
    K : Double, 
    callPut : CallOrPut, 
    F : Double, 
    r : Double, 
    vol : Double, 
    nDays : Int
  ){
    val marketDay = Day(2010, 1, 1).endOfDay
    val exerciseDay = marketDay.day + nDays
    val T = exerciseDay.endOfDay.timeSince(marketDay)

    val fdSolver = new CrankNicholsonOptionPricer(
    DefaultValuationParameters.copy(width = M, timeStepBuilder = new FixedTimesBuilder(M + 1)),
     marketDay, exerciseDay,
     F,
     vol,
     r, callPut, K
    )
    val fdValue = exerciseType match {
      case American => fdSolver.valueAmerican()
      case European => fdSolver.valueEuropean()
    }


    val (ca, ce, bs) = new JonsCrankNicolson(M).values(callPut, F, K, vol, r, T)
    val expectedOptionPrice = exerciseType match {
      case American => ca
      case European => ce
    }
      
    assertEquals(fdValue, expectedOptionPrice, 1e-9)
  }

  @DataProvider(name = "deepITMPutProvider")
  def deepITMPutProvider = {
    def makeArray(M : Int = 100, K : Double, F : Double, r : Double, vol : Double, nDays : Int) = {
      Array[Any](M, K, F, r, vol, nDays)
    }
    Array(
      makeArray(K = 100.0, F = 16.0, r = 0.15, vol = 0.5, nDays = 400)
      ,makeArray(K = 400.0, F = 56.0, r = 605, vol = 0.05, nDays = 60)
      ,makeArray(K = 100.0, F = 66.0, r = 0.35, vol = 0.2, nDays = 400)
      ,makeArray(K = 400.0, F = 76.0, r = 0.45, vol = 0.05, nDays = 60)
      )
    
  }
  @Test(dataProvider = "deepITMPutProvider")
  def testDeepITMAmericanPutHasIntrinsicValue(
    M : Int,
    K : Double,
    F : Double,
    r : Double,
    vol : Double,
    nDays : Int
  ){
    val marketDay = Day(2010, 1, 1).endOfDay
    val exerciseDay = marketDay.day + nDays
    val T = exerciseDay.endOfDay.timeSince(marketDay)

    val fdValue = new CrankNicholsonOptionPricer(
     DefaultValuationParameters.copy(width = M, timeStepBuilder = new FixedTimesBuilder(M + 1)),
     marketDay, exerciseDay,
     F,
     vol,
     r, Put, K
    ).valueAmerican()

    val (ca, ce, bs) = new JonsCrankNicolson(M).values(Put, F, K, vol, r, T)
    assertEquals(ca, fdValue, 1e-9)
    
    val intrinsic = K - F
    assertEquals(intrinsic, fdValue, fdValue * 1e-4)
    
  }

  @DataProvider(name="testEuropeanDeltaAndGammaProvider")
  def testEuropeanDeltaAndGammaProvider = {
    def makeArray(M : Int = 200, K : Double, callPut : CallOrPut, F : Double, r : Double, vol : Double, nDays : Int) = {
      Array(M, K, callPut, F, r, vol, nDays)
    }
    Array(
       makeArray(K = 100.0, callPut = Call, F = 103.0, r = 0.0, vol = 0.5, nDays =  400)
      ,makeArray(K = 100.0, callPut = Put, F = 96.0, r = 0.05, vol = 0.5, nDays =  400)
      ,makeArray(K = 102.0, callPut = Put, F = 96.0, r = 0.05, vol = 0.05, nDays =  60)
      ,makeArray(K = 100.0, callPut = Call, F = 103.0, r = 0.0, vol = 0.5, nDays =  400)
      ,makeArray(K = 100.0, callPut = Put, F = 96.0, r = 0.05, vol = 0.5, nDays =  400)
      ,makeArray(K = 102.0, callPut = Put, F = 96.0, r = 0.05, vol = 0.05, nDays =  60)
      )
  }
  @Test(dataProvider = "testEuropeanDeltaAndGammaProvider")
  def testEuropeanDeltaAndGamma(
    M : Int, 
    K : Double, 
    callPut : CallOrPut, 
    F : Double, 
    r : Double, 
    vol : Double, 
    nDays : Int
  ){
    val marketDay = Day(2010, 1, 1).endOfDay
    val exerciseDay = marketDay.day + nDays
    val T = exerciseDay.endOfDay.timeSince(marketDay)


    def bsPricer(price : Double) : Double = {
      BlackScholes.undiscountedOptionPrice(price, K, callPut, T, vol) * exp( - r * T)
    }


    val bsDeltaFn = new NumericalDerivative(bsPricer, 1e-04)

    def fdPriceFn(price : Double) = new CrankNicholsonOptionPricer(
      DefaultValuationParameters.copy(width = M, timeStepBuilder = new FixedTimesBuilder(M + 1)),
      marketDay, exerciseDay,
     price,
     vol,
     r, callPut, K).valueEuropean()

    var dP = new CrankNicholsonOptionPricer(
      DefaultValuationParameters.copy(width = M, timeStepBuilder = new FixedTimesBuilder(M + 1)),
      marketDay, exerciseDay,
     F,
     vol,
     r, callPut, K
    ).dP
    val fdDeltaFn = new NumericalDerivative(fdPriceFn, dP)
    val fdDelta2 = fdDeltaFn(F)
    val deltaDiff2 = (bsDeltaFn(F) - fdDelta2).abs

    // test delta within 1/10th of 1%
    assertEquals(fdDelta2, bsDeltaFn(F), bsDeltaFn(F).abs * 1e-3)

    // test gamma within 1%
    val bsGammaFn = new NumericalDerivative(bsPricer, 0.02).differentiate
    val fdGammaFn = new NumericalDerivative(fdPriceFn, dP).differentiate
    val fdGamma = fdGammaFn(F)
    assertEquals(fdGamma, bsGammaFn(F), bsGammaFn(F).abs * 1e-2)
  }


  @DataProvider(name = "testAmericanCallUndiscountedProvider")
  def testAmericanCallUndiscountedProvider = {
    Array(
      Array[Any](103.0, 100.0, 1.0, 200)
      ,Array[Any](57.0, 58.0, 1.0, 20)
      )
  }
  @Test(dataProvider = "testAmericanCallUndiscountedProvider")
  def testAmericanCallUndiscountedHasSamePriceAsEuropean(F : Double, K : Double, sigma : Double, nDays : Int){

    val marketDay = Day(2010, 1, 1).endOfDay
    val exerciseDay = marketDay.day + nDays
    val callPut = Call
    def discFn(day : Day) = 1.0

    val D = 5
    val M = 100
    for (
      //D <- 1 to 3 by 2;
      //M <- 80 to 100 by 10;
      callPut <- List(Call, Put)
    ) {
      val americanValue = new CrankNicholsonOptionPricer(
        DefaultValuationParameters.copy(
          timeStepBuilder = new DayTimesBuilder(D), width = M
        ),
        marketDay, exerciseDay, F, sigma, 0.0, callPut, K).valueAmericanWithCorrection()
      //val americanValue = new AmericanOptionValuation(dayStep = D, width = 2 * M + 1)(marketDay, exerciseDay, callPut, discFn, F, K, sigma).value()
      val T = exerciseDay.endOfDay.timeSince(marketDay)
      val europeanValue = BlackScholes.undiscountedOptionPrice(F, K, callPut, T, sigma)
      //val cnValue = new JonsCrankNicolson().valueUndiscounted(callPut, F, K, sigma, T)


      //assertEquals(americanValue, cnValue, cnValue * 5e-4)
      assertEquals(americanValue, europeanValue, europeanValue * 1e-5)
      val europeanDeltaFn = new NumericalDerivative(
        {x : Double => BlackScholes.undiscountedOptionPrice(x, K, callPut, T, sigma)},
        0.01
      )
      val americanDeltaFn = new NumericalDerivative(
        //{x : Double => new AmericanOptionValuation2(dayStep = D, width = 2 * M + 1)(marketDay, exerciseDay, callPut, discFn, x, K, sigma).value()},
        {x : Double => new CrankNicholsonOptionPricer(
          DefaultValuationParameters,
          marketDay, exerciseDay, x, sigma, 0.0, callPut, K).valueAmericanWithCorrection()},
        0.01
      )

      for (F <- (K min F) - 3.0 to (K max F) + 3.0 by 0.2){
        val americanDelta = americanDeltaFn(F)
        val europeanDelta = europeanDeltaFn(F)
        assertEquals(americanDeltaFn(F), europeanDeltaFn(F), 5e-4)
      }

    }
  }

  @Test
  def testDeepITMPutIsWorthIntrinsic{
    val marketDay = Day(2010, 1, 1).endOfDay
    val exerciseDay = marketDay.day + 50
    val callPut = Put
    val z = 1.25
    def discFn(day : Day) = math.exp(- z * day.daysSinceInYears(marketDay.day))

    val F = 20.0
    val K = 80.0
    val sigma = 0.5

    val americanValue = new CrankNicholsonOptionPricer(
      DefaultValuationParameters,
      marketDay, exerciseDay,
      F, sigma, z, callPut, K
    ).valueAmericanWithCorrection()
    val T = exerciseDay.endOfDay.timeSince(marketDay)
    val europeanValue = discFn(exerciseDay) * BlackScholes.undiscountedOptionPrice(F, K, callPut, T, sigma)

    assertEquals(americanValue, K - F, americanValue * 5e-4)
    assertEquals(europeanValue, (K - F) * discFn(exerciseDay), europeanValue * 5e-4)
  }

  //@Test(groups = Array("performance"))
  def testSpeed{
    val marketDay = Day(2010, 1, 1).endOfDay
    val exerciseDay = marketDay.day + 200
    val T = exerciseDay.endOfDay.timeSince(marketDay)
    val K = 100.0
    val vol = 0.5
    val r = 0.05
    val M = 200


    def fdPriceFn(price : Double) = new CrankNicholsonOptionPricer(
      DefaultValuationParameters.copy(width = M, timeStepBuilder = new FixedTimesBuilder(M + 1)),
      marketDay, exerciseDay,
     price,
     vol,
     r, Put, K).valueEuropean()

    import starling.utils.Stopwatch
    val sw = new Stopwatch
    val values = (0 to 200).toList.map{_ => fdPriceFn(100.0)}
    assertTrue(sw.ms < 3000, "This test takes around 2 seconds on Alex's desktop, took " + sw.ms)
  }
}

