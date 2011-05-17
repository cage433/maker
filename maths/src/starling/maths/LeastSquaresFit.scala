package starling.maths

import cern.colt.matrix.linalg.SingularValueDecomposition
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import starling.quantity.Percentage
import cern.colt.matrix.{DoubleMatrix2D, DoubleMatrix1D, DoubleFactory2D, DoubleFactory1D}
import scala.math._
import math.pow
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, DenseDoubleMatrix1D}

object LeastSquaresFit {

  private def fitCoeffs(designMatrix: DoubleMatrix2D, b : DoubleMatrix1D): Array[Double] = {
    // Constuct b vector such that b = y[i] / tol[i]
//    val b = DoubleFactory1D.dense.make(ys.toArray) // TODO scale by tols

    // Perform singular value decomposition
    val svd = new SingularValueDecomposition(designMatrix)

    // Extract matrices
    val U = svd.getU
    val V = svd.getV
    val w = svd.getSingularValues

    var UdotBoverW = DoubleFactory1D.dense.make(designMatrix.columns)
    for (i <- 0 until designMatrix.columns)
      if (w(i) != 0.0)
      // set to U(i).b / w(i)
        UdotBoverW.set(i, U.viewColumn(i).zDotProduct(b) / w(i))

    var coeffs: DoubleMatrix1D = V.zMult(UdotBoverW, null)
    coeffs.toArray
  }
  private def fitCoeffs(designMatrix: DoubleMatrix2D, yValues : List[Double]): Array[Double] = {
    val v = new DenseDoubleMatrix1D(yValues.toArray)
    fitCoeffs(designMatrix, v)
  }

  def fitPolynomialCoefficients(polyOrder: Int, dataPoints: Map[Double, Double]): Array[Double] = {
    if (dataPoints.isEmpty)
      return Array[Double]()
    // TODO Handle tolerances
    //val tolerances = Array.

    val (xs, ys) = dataPoints.unzip.asInstanceOf[Tuple2[List[Double], List[Double]]]

    // First construct the design matrix with the data points as rows & the powers of x as columns
    val realPolyOrder = polyOrder min (xs.size - 1)
    var designMatrix = DoubleFactory2D.dense.make(xs.size, realPolyOrder + 1)
    designMatrix.viewColumn(0).assign(1.0)
    for (j <- 1 to realPolyOrder) {
      designMatrix.viewColumn(j).assign(
        designMatrix.viewColumn(j - 1) |*| new DenseDoubleMatrix1D(xs.toArray)
      )
    }

    fitCoeffs(designMatrix, ys)
  }
  def fitPolynomialCoefficientsWithZero(polyOrder: Int, xBasis: Double, dataPoints : Map[Double, Double]): Array[Double] = {
    val xs = dataPoints.keySet.toList.toArray
    val ys = xs.map(dataPoints(_))
    val m = new DenseDoubleMatrix2D(xs.size, 2)
    m.viewColumn(0).assign(xs)
    m.viewColumn(1).assign(ys)
    fitPolynomialCoefficientsWithZero(polyOrder, xBasis, m)
  }

  // This function will fit the data in dataPoints, along with the point (xBasis, 0) to the function
  // poly = sum(i = 1 to polyOrder) of a[i] * (x - xBasis) ^ i
  // so a[0] == 0 and hence we pass through (xBasis, 0)
  // See http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/BMVA96Tut/node34.html
  def fitPolynomialCoefficientsWithZero(polyOrder: Int, xBasis: Double, dataPoints : DoubleMatrix2D): Array[Double] = {
    val xValues = dataPoints.viewColumn(0).toArray
    if (xValues.size == 0){
      return Array[Double](0.0)
    }

    val needToAddZeroPoint = ! xValues.contains(xBasis)
    val realXValues = if (needToAddZeroPoint){
      val v = new DenseDoubleMatrix1D(xValues.size + 1)
      v.viewPart(1, xValues.size).assign(xValues)
      v(0) = xBasis
      v
    } else {
      dataPoints.viewColumn(0).copy
    }
    realXValues -= xBasis
    val realPolyOrder = polyOrder min (realXValues.size - 1)
    val designMatrix = new DenseDoubleMatrix2D(realXValues.size, realPolyOrder)
    designMatrix.viewColumn(0).assign(realXValues)
    var col = 1
    while (col < realPolyOrder){
      designMatrix.viewColumn(col).assign(designMatrix.viewColumn(col - 1) |*| realXValues)
      col += 1
    }

    val realYValues = if (needToAddZeroPoint){
      val v = new DenseDoubleMatrix1D(xValues.size + 1)
      v.viewPart(1, xValues.size).assign(dataPoints.viewColumn(1))
      v(0) = 0
      v
    } else {
      dataPoints.viewColumn(1).copy
    }
//    val realDataPoints = if (needToAddZeroPoint){
//      val m = new DenseDoubleMatrix2D(xValues.size + 1, 2)
//      m.viewPart(1, 0, xValues.size, 2).assign(dataPoints)
//    }
//
//
//    val yValues = dataPoints.viewColumn(1).toArray
//
//    val extraRows = (if (needToAddZeroPoint) 1 else 0)
//    val numRows = xValues.size + extraRows
////    val (xs, ys) = (dataPoints + (xBasis -> 0.0)).unzip.asInstanceOf[Tuple2[List[Double], List[Double]]]
//    val realPolyOrder = polyOrder min (numRows - 1)
//
//    // First construct the design matrix with the data points as rows & the powers of x as columns
//    var designMatrix = DoubleFactory2D.dense.make(numRows, realPolyOrder)
//    var i = extraRows
//    while (i < numRows){
//      val deltaDiff = xValues(i - extraRows) - xBasis
//      var z = deltaDiff
//      var j = 0
//      while (j < realPolyOrder){
//        // elements are equal to (x[i] - xBasis) ^ j + 1, add one to power since we want no constant term
//        designMatrix.set(i, j, z) // TODO scale by tols
//        z *= deltaDiff
//        j += 1
//      }
//      i += 1
//    }
//    val ys = if (needToAddZeroPointJerome Dive)
//      0.0 :: yValues.toList
//    else
//      yValues.toList
    fitCoeffs(designMatrix, realYValues)
  }

  def fitPolynomialWithZero(polyOrder: Int, xBasis: Double, dataPoints : DoubleMatrix2D): Double => Double = {

    val coeffs: Array[Double] = fitPolynomialCoefficientsWithZero(polyOrder, xBasis, dataPoints)

    x: Double => {
      val z = x - xBasis
      var value: Double = coeffs.last * z
      var i = coeffs.size - 2
      while (i >= 0){
        value = (value + coeffs(i)) * z
        i -= 1
      }
//      var value = 0.0
//      for (i <- 0 until coeffs.size) {
//        value += coeffs(i) * pow(x - xBasis, i + 1)
//      }
      value
    }
  }


  def fitPolynomial(polyOrder: Int, dataPoints: Map[Double, Double]): Double => Double = {


    val coeffs: Array[Double] = fitPolynomialCoefficients(polyOrder, dataPoints)

    x: Double => {
      var value: Double = 0.0
      for (i <- 0 until coeffs.size) {
        value += coeffs(i) * pow(x, i)
      }
      value
    }
  }

  def fitVolSkew(polyOrder : Int, xBasis : Double, deltasAndSkews : DoubleMatrix2D) : Double => Percentage = {
    val doubleFn = fitPolynomialWithZero(polyOrder, xBasis, deltasAndSkews)
    x : Double => Percentage(doubleFn(x))
  }

  def fitVolSmile(polyOrder : Int, dataPoints : Map[Double, Percentage]) : Double => Percentage = {
    val doubleFn = fitPolynomial(polyOrder, Map[Double, Double]() ++ dataPoints.mapValues(_.decimalValue))
    x : Double => Percentage(doubleFn(x))
  }
  def fitVolSmile(polyOrder : Int, dataPoints : DoubleMatrix2D) : Double => Percentage = {
    val xs = dataPoints.viewColumn(0).toArray
    val ys = dataPoints.viewColumn(1).toArray
    val map = Map[Double, Percentage]() ++ xs.zip(ys).map{case (x, y) => x -> Percentage(y)}
    fitVolSmile(polyOrder, map)
  }
}
