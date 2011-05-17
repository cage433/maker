package starling.maths

import cern.colt.matrix.linalg.CholeskyDecomposition
import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix, DoubleFactory2D}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import java.lang.Math._

/** A utility to create perturbations to a vector of lognormal prices over some time
 * 	step using supplied	vols and correlation matrix.
 */
case class PriceShiftGenerator(
  prices : Vector,
  vols : Vector,
  rhoMatrix : Matrix,
  dt : Double,
  seed : Int
){
  val nRiskFactors = prices.size

  require(vols.size == nRiskFactors, "Need vol for each price")
  require(rhoMatrix.rows == nRiskFactors && rhoMatrix.columns == nRiskFactors, "Mis-sized rho matrix")
  require(dt >= 0, "Negative time interval")


  val lognormalPriceGenerator = new LognormalPrices(prices, vols, rhoMatrix, dt, seed)
 
 	/** Each call to this function generates a new vector of prices
  */
  def nextShifts() : Vector = {
   new DVector(lognormalPriceGenerator.nextValues) - prices
  }

  /** For unit tests - sets vols to be constant
   */
  def setConstantVol(vol : Double) : PriceShiftGenerator = {
    PriceShiftGenerator(prices.copy, vols.copy.assign(vol), rhoMatrix.copy, dt, seed)
  }
  /** For unit tests - sets prices to be constant
   */
  def setConstantPrice(price : Double) : PriceShiftGenerator = {
    PriceShiftGenerator(prices.copy.assign(price), vols.copy, rhoMatrix.copy, dt, seed)
  }
  
  /** For unit tests - sets correlations to be constant
   */
  def setConstantRho(rho : Double) : PriceShiftGenerator = {
    PriceShiftGenerator(prices.copy, vols.copy, MatrixUtils.rhoMatrixWithConstantOffDiagonal(rhoMatrix.rows, rho), dt, seed)
    
  }
  def setSeed(newSeed : Int) : PriceShiftGenerator = {
    PriceShiftGenerator(prices.copy, vols.copy, rhoMatrix.copy, dt, newSeed)
  }
  
  /** For a sanity check while regressin testing - restricts to a range of prices
   */
  def restrictToRange(offset : Int, width : Int) : PriceShiftGenerator = {
    PriceShiftGenerator(
      prices.copy.viewPart(offset, width),
      vols.copy.viewPart(offset, width),
      rhoMatrix.copy.viewPart(offset, offset, width, width),
      dt,
      seed
    )
  }
}
