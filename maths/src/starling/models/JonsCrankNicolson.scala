package starling.models

import starling.utils.conversions.RichColtMatrices._
import cern.colt.matrix.{DoubleMatrix1D => Vector}
import cern.colt.matrix.{DoubleMatrix2D => Matrix}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D => DMatrix}
import math._

/**
 * Pretty much a direct copy of Jon's Crank Nicholson solver in Kudu. Kept for
 * unit testing only.
 */
class JonsCrankNicolson(val numSteps: Int = 200) extends OptionValuation {
  val N = numSteps // time steps
  val M = numSteps // space steps: note they don't have to be the same as time steps.

  private val FD_PRECISION = 1.0e-5

  def valueUndiscounted(callOrPut: CallOrPut, F: Double, K: Double, sigma: Double, T: Double) = {
    value(callOrPut, F, K, sigma, 0.0, T)
  }
  // Trinomial method using a Crank-Nicholson semi-implicit FD scheme with variance reduction
  def value(callPut: CallOrPut, S: Double, K: Double, vol: Double, r: Double, T: Double) = {
    val (amer, eur, bs) = values(callPut, S, K, vol, r, T)
    amer - eur + bs
  }

  // Trinomial method using a Crank-Nicholson semi-implicit FD scheme with variance reduction
  def values(callPut: CallOrPut, S: Double, K: Double, vol: Double, r: Double, T: Double) = {
    val bs = BlackScholes.undiscountedOptionPrice(S, K, callPut, T, vol) * exp(-r * T)
    val volSq = vol * vol
    val nu = -0.5 * volSq
    val dt = T / numSteps

    val dxConfidence = vol * sqrt(T) * sqrt(log(1 / FD_PRECISION)) + abs(nu) * T // space localization
    val dx = 2 * dxConfidence / M
    val edx = exp(dx)

    // Probabilities
    val pu = -0.25 * dt * (volSq / (dx * dx) + nu / dx)
    val pm = 1 + volSq * dt / (2 * dx * dx) + r * dt / 2
    val pd = -0.25 * dt * (volSq / (dx * dx) - nu / dx)

    // Initialize prices at maturity
    var P = new DVector(2 * M + 1)
    var assetPrice = S * exp(-M * dx);
    for (i <- 0 to 2 * M) {
      P.set(i, assetPrice)
      assetPrice *= edx;
    }


    def tridiagonalSolve (a : Vector, b : Vector, c : Vector, d : Vector, x : Vector){
      val n = x.size
      /* Modify the coefficients. */
      c.setQuick(0, c(0) / b(0)) /* Division by zero risk. */
      d.setQuick(0, d(0) / b(0)) /* Division by zero would imply a singular matrix. */
      for (i <- 1 until n) {
        val id = 1 / (b(i) - c(i - 1) * a(i)); /* Division by zero risk. */
        c.setQuick(i, c(i) * id) /* Last value calculated is redundant. */
        d.setQuick(i, (d(i) - d(i - 1) * a(i)) * id)
      }

      /* Now back substitute. */
      x.setQuick(n - 1, d(n - 1))
      for (i <- n - 2 to 0 by -1)
        x.setQuick(i, d(i) - c(i) * x(i + 1))
    }

    // Initialise option payoff
    var C: Matrix = new DMatrix(2, 2 * M + 1)
    var CE: Matrix = new DMatrix(2, 2 * M + 1)
    for (i <- 0 to 2 * M) {
      C.set(0, i, callPut.intrinsicPrice(P.get(i), K).value)
      CE.set(0, i, callPut.intrinsicPrice(P.get(i), K).value)
    }

    // Compute boundary conditions: eliminate upper diagonal
    var lambdaL, lambdaU = 0.0
    callPut match {
      case Call => lambdaL = 0.0; lambdaU = P.get(2 * M) - P.get(2 * M - 1);
      case Put => lambdaL = P.get(0) - P.get(1); lambdaU = 0.0;
    }

    // Step back through the tree
    for (i <- (0 to N - 1).reverse) {
      C = solveCrankNicholsonTridiagonalSystem(C, M, pu, pm, pd, lambdaL, lambdaU)
      CE = solveCrankNicholsonTridiagonalSystem(CE, M, pu, pm, pd, lambdaL, lambdaU)

      // Apply early exercise condition
      for (j <- 0 to 2 * M) {
        CE.set(0, j, CE.get(1, j))
        callPut match {
          case Call => C.set(0, j, max(C.get(1, j), P.get(j) - K))
          case Put => C.set(0, j, max(C.get(1, j), K - P.get(j)))
        }
      }
    }

    (C.get(0, M), CE.get(0, M), bs)
  }

  def solveCrankNicholsonTridiagonalSystem(C: Matrix, M: Int, pu: Double, pm: Double, pd: Double, lambdaL: Double, lambdaU: Double) = {
    var pmp = new DVector(2 * M + 1)
    var pp = new DVector(2 * M + 1)

    // Substitute boundary condition at i = 0 into i = 1
    pmp.set(1, pm + pd)
    pp.set(1, -pu * C.get(0, 2) - (pm - 2.0) * C.get(0, 1) - pd * C.get(0, 0) + pd * lambdaL)

    // Eliminate upper diagonal
    for (i <- 2 to 2 * M - 1) {
      pmp.set(i, pm - pu * pd / pmp.get(i - 1))
      pp.set(i, -pu * C.get(0, i + 1) - (pm - 2.0) * C.get(0, i) - pd * C.get(0, i - 1) - pp.get(i - 1) * pd / pmp.get(i - 1))
    }

    // Use boundary condition at i = 2 * M + 1 and equation at i = 0
    C.set(1, 2 * M, (pp.get(2 * M - 1) + pmp.get(2 * M - 1) * lambdaU) / (pu + pmp.get(2 * M - 1)))
    C.set(1, 2 * M - 1, C.get(1, 2 * M) - lambdaU)

    // Back substitution
    for (i <- (1 to 2 * M - 2).reverse)
      C.set(1, i, (pp.get(i) - pu * C.get(1, i + 1)) / pmp.get(i))

    C.set(1, 0, C.get(1, 1) - lambdaL)

    C
  }
}
