package starling.maths

import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix, DoubleFactory2D}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D =>DVector, DenseDoubleMatrix2D => DMatrix}
import cern.colt.matrix.linalg.{Algebra, CholeskyDecomposition}
import cern.jet.math.Functions
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import math.{sqrt, pow}

object MatrixUtils {

  /** The rho matrix m may not be SPD. Normally due to rounding errors
   *  in the calculation of historical correlations. If this is the case
   *  then reducing the off diagonal terms by a small fraction will normally repair the problem
   */
  def makeCholeskyDecomp(rhoMatrix : Matrix) = {
    assert(rhoMatrix.rows > 0, "Empty rho matrix")
    def makeRidgeAdjustment(m : Matrix) : Matrix = {
      val adjustedMatrix = m.copy
      adjustedMatrix *= 0.99999
      for (i <- 0 until m.rows){
        adjustedMatrix(i, i) = 1.0
      }
      adjustedMatrix
    }
    var cd = new CholeskyDecomposition(rhoMatrix.copy)

    if (! cd.isSymmetricPositiveDefinite){
      cd = new CholeskyDecomposition(makeRidgeAdjustment(rhoMatrix.copy))
      assert(cd.isSymmetricPositiveDefinite, "Rho matrix is non SPD")
    }
    cd
  }

  /** Build random prices, vols and rhos. for unit tests
   */
	def randomPriceStatistics(nPricePoints : Int, seed : Int) : (Vector, Vector, Matrix) = {
	  val su = RandomVariables.standardUniform(seed)
	  val prices = new DVector(nPricePoints).assign(su) * 100.0
	  val vols = new DVector(nPricePoints).assign(su)
	  val rhoMatrix = RandomCorrelationMatrix(nPricePoints, 2 * seed)
	  (prices, vols, rhoMatrix)
	}
 
  /** Build a constant rho motrix. For unit tests.
   */
	def rhoMatrixWithConstantOffDiagonal(size : Int, rho : Double) : Matrix = {
	  val m = DoubleFactory2D.dense.identity(size)
	  for (
	    i <- 0 until size;
	    j <- 0 until size;
	    if i != j
	  ){
	    m(i, j) = rho
	  }
	  m
	}
 
	/** Returns the sum of a sequence of vectors
  */
	def sumVectors(vs : Seq[Vector]) : Vector = {
	  if (vs.isEmpty)
     new DVector(0)
   else {
     val n = vs.head.size
     (new DVector(n).asInstanceOf[Vector] /: vs)(_+_)
   }
	} 
}

/** Builds a random correlation matrix of the specified size.
* 	Algorithm taken from the 'Handbook of Computational Statistics: Concepts and Methods', p359'
*/
object RandomCorrelationMatrix{

	def apply(N_factors : Int) : Matrix = apply(N_factors, RandomVariables.timeAsSeed)



	def apply(N_factors : Int, seed : Int) : Matrix = {
    val F = Functions.functions
    val FAC = DoubleFactory2D.dense
    val sn = RandomVariables.standardNormal(seed)
    val su = RandomVariables.standardUniform(2 * seed)
    def identityMatrix() : Matrix = {
      FAC.diagonal(new DVector(N_factors).assign(1.0))
    }

    val I = identityMatrix

    def randomEigenVector() : Vector = {
      val v =  new DVector(N_factors).assign(su)
      v *= (N_factors / v.zSum())
      v
    }

    val C = FAC.diagonal(randomEigenVector())
    val I_minus_C = I - C
    val A = Algebra.DEFAULT



    def normalVector() : Vector = {
      new DVector(N_factors).assign(sn)
    }

    /*
    * returns x^T * M * y
    */
    def product(x : Vector, M : Matrix, y : Vector) = {
      x.zDotProduct(M.zMult(y, null))
    }

    def plusOrMinusOne() : Double = {
        if (sn.nextDouble > 0) 1.0 else - 1.0
    }

    if (N_factors == 1)
      return new DMatrix(1, 1).assign(1.0)


		var tryCount = 0
		val V = new DMatrix(N_factors, N_factors)
		var E = identityMatrix()
		var i_row = 0
		while (i_row < N_factors){
			var x = E * normalVector()
			val y = E * normalVector()
			val a = product(x, I_minus_C, x)
			val b = product(x, I_minus_C, y)
			val c = product(y, I_minus_C, y)

			val e_squared = b * b - a * c
			if (e_squared < 0){
				tryCount += 1
				if (tryCount > 1000)
					throw new IllegalStateException("Too many tries to make correlation matrix")
			} else {
				val alpha = (b + sqrt(e_squared) * plusOrMinusOne()) / a
				val r = x * alpha - y
				val v_k = r * (plusOrMinusOne() / sqrt(r.zDotProduct(r)))
				E -= A.multOuter(v_k, v_k, null)
				V.viewRow(i_row).assign(v_k.copy())
				if (i_row == N_factors - 2){
					x = E * normalVector * (pow(x.zDotProduct(x), -0.5))
					V.viewRow(N_factors - 1).assign(x)
				}
				i_row += 1
			}
		}
		val rhoMatrix = V * C * V.viewDice

		/*
		* Rounding errors caused the matrix to be not quite symmetric. We correct for that
		* here by copying the upper part to the lower.
		*/
		for (i <- 0 until N_factors)
			for (j <- 0 until i)
				rhoMatrix(i, j) = rhoMatrix(j, i)
		rhoMatrix
	}

}


