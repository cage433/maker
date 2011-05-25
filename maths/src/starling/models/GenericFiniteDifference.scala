package starling.models
                                                                                                                                                   

import math._
import cern.colt.matrix.{DoubleMatrix2D => Matrix, DoubleMatrix1D => Vector, DoubleFactory2D}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D => DMatrix, DenseDoubleMatrix1D => DVector}
import cern.colt.matrix.linalg.LUDecompositionQuick
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import starling.maths.CubicSpline


/**
 * This isn't currently used, however was very useful when investigating the accuracy
 * of Jon's finite difference algorithm. Supports Implicit, Explicit, CN algorithms and
 * also different boundary conditions (contant gradient etc). I'll keep it until
 * this branch is in trunk. 
 */
object GenericFiniteDifference{

  def dC_dz(width : Int, dz : Double) : Matrix = {
    val m = new DMatrix(width, width)
    for (i <- 1 until width - 1){
      m(i, i - 1) = -1.0 / (2 * dz)
      m(i, i + 1) = 1.0 / (2 * dz)
    }
    m
  }


  def d2C_dz2(width : Int, dz : Double) : Matrix = {
    val m = new DMatrix(width, width)
    for (i <- 1 until width - 1){
      m(i, i - 1) = 1.0 / (dz * dz)
      m(i, i) = -2.0 / (dz * dz)
      m(i, i + 1) = 1.0 / (dz * dz)
    }
    m
  }
  /**
    Black-Scholes PDE is

      df/dt + (r - q) * S * df/dS + 0.5 * sigma^2 * S^2 * d2f/dS^2 = r * f

    forward prices behave as though q = r, hence after transformation S = exp(z) this becomes

      df/dt - 0.5 * v^2 * df/dz + 0.5 * sigma^2 * d2f/dz^2 = r * f
  */
  def build_zMatrix(width : Int, v : Double, dz : Double) : Matrix = {
    dC_dz(width, dz) * (-0.5 * v * v) + (d2C_dz2(width, dz) * (0.5 * v * v))
  }

}

import GenericFiniteDifference._

trait FDMethod{
  def id(width : Int) = DoubleFactory2D.dense.identity(width) 
  def build_U_V(width : Int, r : Double, v : Double, dz : Double, dt : Double) : (Matrix, Matrix) 
}
case object Implicit extends FDMethod{
  def build_U_V(width : Int, r : Double, v : Double, dz : Double, dt : Double) : (Matrix, Matrix) = {
    val Z = build_zMatrix(width, v, dz)
    val U = id(width) / dt 
    val V = id(width) * (1 / dt + r) + 
      ( dC_dz(width, dz)  - d2C_dz2(width, dz) ) * 0.5 * v * v
    (U, V)
  }
}
case object Explicit extends FDMethod{
  def build_U_V(width : Int, r : Double, v : Double, dz : Double, dt : Double) : (Matrix, Matrix) = {
    val Z = build_zMatrix(width, v, dz)
    val U = DoubleFactory2D.dense.identity(width) * (1 / dt - r) -
      ( dC_dz(width, dz)  - d2C_dz2(width, dz) ) * 0.5 * v * v
    val V = DoubleFactory2D.dense.identity(width) * (1 / dt)
    (U, V)
  }
}
case object CrankNicholson extends FDMethod{
  def build_U_V(width : Int, r : Double, v : Double, dz : Double, dt : Double) : (Matrix, Matrix) = {
    val (e_U, e_V) = Explicit.build_U_V(width, r, v, dz, dt)
    val (i_U, i_V) = Implicit.build_U_V(width, r, v, dz, dt)
    ((e_U + i_U) * 0.5, (e_V + i_V) * 0.5)
  }
}

trait BoundaryCondition
case object IntrinsicBC extends BoundaryCondition
case object IntrinsicGradientBC extends BoundaryCondition


class GenericFiniteDifference(
  M : Int, 
  zWidthOrNone : Option[Double], 
  times : Array[Double],
  zeroRate : Double,
  method : FDMethod,
  bc : BoundaryCondition,
  sigma : Double,
  S : Double,
  payoffFn : (Double, Double) => Double,
  americanConditions : (Double, Double, Double) => Double
)
{
  val T = times.last
  val width = 2 * M + 1

  val zWidth = zWidthOrNone match {
    case Some(s) => s
    case None =>
      // Taken from Jon's implementation - seems to give more accurate results than
      // arbitrarily chosen numbers
      val FD_PRECISION = 1e-5
      val nu = -0.5 * sigma * sigma
      2 * (sigma * sqrt(T) * sqrt(log(1 / FD_PRECISION)) + abs(nu) * T) 
  }

  val (zs, dz) = {
    val zLow = - zWidth 
    val zHigh = zWidth 
    val dz = (zHigh - zLow) / (width - 1)
    (
      new DVector((0 until width).toArray.map(zLow + _ * dz)),
      dz
    )
  }

  var U_Vs : List[(Double, Matrix, Matrix)] = Nil

  def buildUV(dt : Double) : (Matrix, Matrix) = {
   U_Vs.find{
      case (time, u, v) => (time - dt).abs < 1e-8 
      case _ => false
    } match {
      case Some((t, u, v)) => (u.copy, v.copy)
      case None => {
        val (u, v) = method.build_U_V(width, zeroRate, sigma, dz, dt) 
        U_Vs = (dt, u, v) :: U_Vs
        (u.copy, v.copy)
      }
    }
  }

  val prices = zs.map{x : Double => S * exp(x)}

  def diffuse(C : Vector, t1 : Double, t2 : Double) : Vector = {
    val dt = t2 - t1
    val (_U, _V) = buildUV(dt)
    //val (_U, _V) = method.build_U_V(width, zeroRate, sigma, dz, dt)
    val pd = -0.25  * ( (sigma * sigma / (dz * dz) - (-0.5 * sigma * sigma) / dz))
    val pm = 1 / dt + sigma * sigma / (2.0 * dz * dz) + zeroRate / 2.0
    val pu = -0.25  * ( (sigma * sigma / (dz * dz) + (-0.5 * sigma * sigma) / dz))
    //val q = -(pm - 2 / dt)
    var _C = _U * C
    bc match {
      case IntrinsicBC => {
        _C(0) = payoffFn(t1, prices(0))
        _C(width - 1) = payoffFn(t1, prices(width - 1))
        _V(0, 0) = 1.0
        _V(width - 1, width - 1) = 1.0
      }
      case IntrinsicGradientBC => {
        _C(0) = payoffFn(t1, prices(1)) - payoffFn(t1, prices(0))
        _C(width - 1) = payoffFn(t1, prices(width - 1)) - payoffFn(t1, prices(width - 2))
        _V(0, 0) = -1.0
        _V(0, 1) = 1.0
        _V(width - 1, width - 2) = -1.0
        _V(width - 1, width - 1) = 1.0
      }
    }
    val lud = new LUDecompositionQuick()
    lud.decompose(_V, 2)
    lud.solve(_C)
    _C
  }

  def solver : (Double, Double, Double) = {
   var C = prices.map{ p : Double => payoffFn(times.last, p)}
   for ((t1, t2) <- times.zip(times.tail).reverse){
     C = diffuse(C, t1, t2)
     for(i <- 0 until width)
       C(i) = americanConditions(prices(i), t1, C(i))
   }
   val spline = new CubicSpline(prices, C)
   val dx = 0.1
   (spline (S - dx), spline(S), spline(S + dx))
  }
  def solve : Double = solver._2
}
