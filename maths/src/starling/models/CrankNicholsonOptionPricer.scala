package starling.models

import starling.daterange.Day
import math._
import starling.daterange.DayAndTime

// Trinomial method using a Crank-Nicholson semi-implicit FD scheme with variance reduction
// AMc - Code seems to have come from a book by Clewlow/Strickland
// This has been hacked at somewhat to improve performance-parti in
case class CrankNicholsonOptionPricer(
  fdParameters : EnvironmentParameters, times : Array[Double], S: Double, vol: Double, r: Double, callPut : CallOrPut, K : Double)
{
  def this (fdParameters : EnvironmentParameters, marketDay : DayAndTime, exerciseDay : Day, S : Double, vol : Double, r : Double, callPut : CallOrPut, K : Double) = this(
    fdParameters, fdParameters.timeStepBuilder.times(marketDay, exerciseDay), S, vol, r, callPut, K
  )

  val T = times.last
  val width = fdParameters.width
  val volSq = vol * vol
  val nu = -0.5 * volSq

  val dx = fdParameters.dz(vol, T)

  // Initialize prices at maturity
  val gridSize = 2 * width + 1
//  val P : Array[Double] = (0 to 2 * width).toArray.map{ i : Int => S * exp((- width + i) * dx)}

  val P = Array.ofDim[Double](gridSize)
  val intrinsic = Array.ofDim[Double](gridSize)
  val probExercisePayoffs = Array.ofDim[Double](gridSize)
  val priceRatioPayoffs = Array.ofDim[Double](gridSize)
  var i = 0
  while (i < gridSize) {
    val price = S * exp((- width + i) * dx)
    P(i) = price
    intrinsic(i) = callPut match {
      case Call => (price - K) max 0.0
      case Put => (K - price) max 0.0
    }
    if (intrinsic(i) > 0.0) {
      probExercisePayoffs(i) = 1.0
      priceRatioPayoffs(i) =price / S
    }
    i += 1
  }
  lazy val dP = 0.5 * (P(width + 1) - P(width - 1))
  val N_times = times.size
  val N_timeSteps = N_times - 1
  val dts = Array.ofDim[Double](N_timeSteps)
  i = 0
  while (i < N_timeSteps){
    dts(i) = times(N_times - 1 - i) - times(N_times - 2 - i)
    i += 1
  }

  //  val intrinsic : Array[Double] = P.map(callPut.intrinsic(K)_) //(price : Double) = callPut.intrinsic(K)(price)
//  def isInTheMoney(price : Double) = intrinsic(price) > 0
//  def probExercisePayoff(price : Double) = if (isInTheMoney(price)) 1.0 else 0.0
//  val probExercisePayoffs = P.map(probExercisePayoff)
//  def priceRatioPayoff(price : Double) = if (isInTheMoney(price)) price / S else 0.0
//  val priceRatioPayoffs = P.map(priceRatioPayoff)
  
  def optionValue(x : Array[Double]) : Double = {
    val Array(probOfExercise, priceRatio, _) = x
    optionValue(probOfExercise, priceRatio)
  }
  def optionValue(probOfExercise : Double, priceRatio : Double) : Double = {
    callPut match {
      case Call => (S * priceRatio - K * probOfExercise)
      case Put => (- S * priceRatio + K * probOfExercise)
    }
  }

  def assets(isAmerican : Boolean) = {


    def diffuse(C: Array[Double], payoffs : Array[Double], dt : Double, r : Double){
      // Compute boundary conditions: eliminate upper diagonal
//      val lambdaL = payoff(P(1)) - payoff(P(0))
//      val lambdaU = payoff(P(2 * width)) - payoff(P(2 * width - 1))
      val lambdaL = payoffs(1) - payoffs(0)
      val lambdaU = payoffs(2 * width) - payoffs(2 * width - 1)

      // Probabilities
      val pu = -0.25 * dt * (volSq / (dx * dx) - 0.5 * volSq / dx)
      val pm = 1 + volSq * dt / (2 * dx * dx)  + r * dt / 2
      val pd = -0.25 * dt * (volSq / (dx * dx) + 0.5 * volSq / dx)

      var pmp = new Array[Double](2 * width + 1)
      var pp = new Array[Double](2 * width + 1)

      // Substitute boundary condition at i = 0 into i = 1
      pmp(1) = pm + pd
      pp(1) = -pu * C(2) - (pm - 2.0) * C(1) - pd * C(0) + pd * lambdaL

      // Eliminate upper diagonal
      var i : Int = 2
      while (i <= 2 * width - 1) {
//      for (i <- 2 to 2 * width - 1) {
        pmp(i) = pm - pu * pd / pmp(i - 1)
        pp(i) = -pu * C(i + 1) - (pm - 2.0) * C(i) - pd * C(i - 1) - pp(i - 1) * pd / pmp(i - 1)
        i += 1
      }

      // Use boundary condition at i = 2 * width + 1 and equation at i = 0
      C(2 * width) = (pp(2 * width - 1) + pmp(2 * width - 1) * lambdaU) / (pu + pmp(2 * width - 1))
      C(2 * width - 1) = C(2 * width) - lambdaU

      // Back substitution
      i = 2 * width - 2
      while (i >= 1){
//      for (i <- 2 * width - 2 to 1 by -1)
        C(i) = (pp(i) - pu * C(i + 1)) / pmp(i)
        i -= 1
      }

      C(0) = C(1) - lambdaL
    }

    // Initialise option payoff
    var probabilityOfExercise = probExercisePayoffs.clone// P.map(probExercisePayoff)
    var undiscountedProbabilityOfExercise = probExercisePayoffs.clone//P.map(probExercisePayoff)
    var priceRatioGivenExercise = priceRatioPayoffs.clone//P.map(priceRatioPayoff)

    // Step back through the tree
    var i_timeStep = 0
    while (i_timeStep < N_timeSteps){
      val dt = dts(i_timeStep)
      diffuse(probabilityOfExercise, probExercisePayoffs, dt, r)
      diffuse(priceRatioGivenExercise, priceRatioPayoffs, dt, r)
      diffuse(undiscountedProbabilityOfExercise, probExercisePayoffs, dt, 0)

      // Apply early exercise condition
      if (isAmerican) {
        var i: Int = 0
        while (i < 2 * width + 1) {
          if (intrinsic(i) > optionValue(probabilityOfExercise(i), priceRatioGivenExercise(i))) {
            probabilityOfExercise(i) = 1.0
            priceRatioGivenExercise(i) = P(i) / S
            undiscountedProbabilityOfExercise(i) = 1.0
          }
          i += 1
        }
      }
      i_timeStep += 1
    }
//    for ((from, to) <- times.zip(times.tail).reverse) {
//      val dt = to - from
//      diffuse(probabilityOfExercise, probExercisePayoffs, dt, r)
//      diffuse(priceRatioGivenExercise, priceRatioPayoffs, dt, r)
//      diffuse(undiscountedProbabilityOfExercise, probExercisePayoffs, dt, 0)
//
//      // Apply early exercise condition
//      if (isAmerican) {
//        var i: Int = 0
//        while (i < 2 * width + 1) {
//          if (intrinsic(i) > optionValue(probabilityOfExercise(i), priceRatioGivenExercise(i))) {
//            probabilityOfExercise(i) = 1.0
//            priceRatioGivenExercise(i) = P(i) / S
//            undiscountedProbabilityOfExercise(i) = 1.0
//          }
//          i += 1
//        }
//      }
//    }

    Array(probabilityOfExercise(width), priceRatioGivenExercise(width), undiscountedProbabilityOfExercise(width))
  }

  def americanAssetsWithCorrection = {
    val Array(aProbEx, aPriceRatio, aUndiscProbEx) = assets(isAmerican = true)
    val Array(eProbEx, ePriceRatio, eUndiscProbEx) = assets(isAmerican = false)
    val bs = new BlackScholes(S, K, callPut, T, vol)
    val disc = exp(-r * T)
    val (bsProbEx, bsPriceRatio, bsUndiscProbEx) = (
      bs.probabilityOfExercise * disc,
      bs.expectedPriceFractionGivenExercise * disc,
      bs.probabilityOfExercise
    )
    val probEx = aProbEx - (eProbEx - bsProbEx)
    val undiscProbEx = aUndiscProbEx - (eUndiscProbEx - bsUndiscProbEx)
    val priceRatio = aPriceRatio - (ePriceRatio - bsPriceRatio)
    Array(probEx, priceRatio, undiscProbEx)
  }

  def valueEuropean() = optionValue(assets(isAmerican = false))
  def valueAmerican() = optionValue(assets(isAmerican = true))
  def valueAmericanWithCorrection() = optionValue(americanAssetsWithCorrection)

}

