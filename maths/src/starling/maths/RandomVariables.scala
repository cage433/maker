package starling.maths

import cern.jet.random.engine.MersenneTwister
import cern.jet.random.{Uniform, Normal, EmpiricalWalker}
import cern.colt.matrix.linalg.CholeskyDecomposition
import cern.jet.random.Empirical.NO_INTERPOLATION
import cern.colt.matrix.{DoubleMatrix2D => Matrix, DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D => DMatrix, DenseDoubleMatrix1D => DVector}
import starling.utils.conversions.RichColtMatrices._

import math._
import org.apache.commons.math.distribution.{NormalDistribution, NormalDistributionImpl}

object RandomVariables {
	def standardUniform() : Uniform = standardUniform(12345)
	def standardUniform(seed : Int) : Uniform = new Uniform(new MersenneTwister(seed))
	
  def normal(mean : Double, stDev : Double) : Normal = normal(mean, stDev, 12345)
  def normal(mean : Double, stDev : Double, seed : Int) : Normal = new Normal(mean, stDev, new MersenneTwister(seed))

  def standardNormal(seed : Int) : Normal = new Normal(0.0, 1.0, new MersenneTwister(seed))

  /**
   * Different implementation to standardNormal - uses apache commons maths
   */
  def standardNormal2 : NormalDistribution = new NormalDistributionImpl(0.0, 1.0)

  private var seedIncrement = 0
	def timeAsSeed() : Int = {
    seedIncrement += 1
    (System.currentTimeMillis() % java.lang.Integer.MAX_VALUE + seedIncrement).asInstanceOf[Int]
  }

	def uniformDiscrete(n : Int, seed : Int) : EmpiricalWalker ={ 
		new EmpiricalWalker(Array.fill(n)(1.0), NO_INTERPOLATION, new MersenneTwister(seed))                                 
  }
} 

/**
 * Used to create random object in unit tests
 */
class RandomThing[A](list : List[A], seed : Int = 934037){
  val u = RandomVariables.uniformDiscrete(list.size, seed)
  def next : A = list(u.nextInt)
}

trait RandomVariable{
  def nextDouble() : Double
}

class StandardUniformVariable(seed : Int) extends RandomVariable{
  val su = new Uniform(new MersenneTwister(seed))
  def nextDouble = su.nextDouble
}

class StandardNormalVariable(seed : Int) extends RandomVariable{
  val sn = new Normal(0.0, 1.0, new MersenneTwister(seed))

  def nextDouble = sn.nextDouble
}

object StandardNormal{
  private val n = RandomVariables.standardNormal(12345)
  def pdf(x : Double) = n.pdf(x)
  def cdf(x : Double) = n.cdf(x)
}

class NormalVariable(mean : Double, stDev : Double, seed : Int) extends RandomVariable{
  val sn = new StandardNormalVariable(seed)

  def nextDouble = mean + stDev * sn.nextDouble
}

trait RandomVector{
  def nextValues() : Array[Double]
}

class UncorrelatedStandardNormals(n : Int, seed : Int) extends RandomVector{
   val sn = new StandardNormalVariable(seed)

  def nextValues = Array.fill[Double](n){sn.nextDouble}
}

class CorrelatedStandardNormals(rhoMatrix : Matrix, seed : Int) extends RandomVector{
  def this(rho : Double, seed : Int) = this(MatrixUtils.rhoMatrixWithConstantOffDiagonal(2, rho), seed)

  val cholesky= {
    // The rho matrix m may not be SPD. Normally due to rounding errors
    // in the calculation of historical correlations. If this is the case
    // then reducing the off diagonal terms by a small fraction will normally repair the problem
    var m = rhoMatrix.copy
    var cd = new CholeskyDecomposition(m)
    if (! cd.isSymmetricPositiveDefinite){
      m *= 0.9999
      for (i <- 0 until m.rows){
        m(i, i) = 1.0
      }
      cd = new CholeskyDecomposition(m)
      if (! cd.isSymmetricPositiveDefinite){
        throw new IllegalStateException("Non SPD matrix")
      }
    }
    cd.getL
  }
  val usn = new UncorrelatedStandardNormals(rhoMatrix.columns, seed)

  def nextValues = {
    (cholesky * new DVector(usn.nextValues)).toArray
  }
}


class LognormalPrices(prices : Vector, vols : Vector, rhoMatrix : Matrix, dt : Double, seed : Int) extends RandomVector{

  def this(F1 : Double, F2 : Double, vol1 : Double, vol2 : Double, rho : Double, dt : Double, seed : Int = 12345) = this(
    new DVector(Array(F1, F2)),
    new DVector(Array(vol1, vol2)),
    MatrixUtils.rhoMatrixWithConstantOffDiagonal(2, rho),
    dt,
    seed
  )
  def this(lhs : Lognormal, rhs : Lognormal, rho : Double, seed : Int) = this(
    lhs.mean, rhs.mean,
    lhs.sigma, rhs.sigma,
    rho,
    1.0,
    seed
  )
  val csn = new CorrelatedStandardNormals(rhoMatrix, seed)


  def nextValues() = {
//    (nextShifts() + prices).toArray
    val dz = csn.nextValues
    val dlogP = new DVector(dz) |*| vols * sqrt(dt)
    val riskAdj = vols |*| vols * 0.5 * dt
    val res = (prices |*| ((dlogP - riskAdj).exp)).toArray
    res
  }

  def nextShifts() = {
    val dz = new DVector(csn.nextValues)
		val dlogP = dz * sqrt(dt) |*| vols
		val riskAdj = vols |*| vols * 0.5 * dt
		val dp : Vector = (dlogP - riskAdj).exp - 1.0
		val priceShifts = prices |*| dp
    priceShifts
  }
}







