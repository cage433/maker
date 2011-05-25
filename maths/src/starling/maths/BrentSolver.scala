package starling.maths

import org.apache.commons.math.analysis._

object BrentSolver {
	def solve(function: (Double) => Double, min: Double, max: Double) = {
		val brentFunction = new BrentFunction(function)
		val brentSolver = new solvers.BrentSolver
		brentSolver.solve(brentFunction, min, max)
	}

	def solve(function: (Double) => Double, min: Double, max: Double, initial: Double) = {
		val brentFunction = new BrentFunction(function)
		val brentSolver = new solvers.BrentSolver
		brentSolver.solve(brentFunction, min, max, initial)
	}
}

class BrentFunction(function: (Double) => Double) extends UnivariateRealFunction {
	def value(x: Double): Double = function(x)
}