package starling.varcalculator

import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import starling.utils.conversions.RichColtMatrices._
import starling.maths.AcklamInverseNormal
import math.sqrt


object ParametricVar{
  def calculate(prices : Vector, deltas : Vector, vols : Vector, rhos : Matrix, dT : Double, centile : Double) : Double = {

    val alpha = AcklamInverseNormal(centile)
    val d = (prices |*| deltas |*| vols * alpha * sqrt(dT)).asColumnMatrix

    val r = d.viewDice * rhos * d

    assert(r.rows == 1 && r.columns == 1)
    -sqrt(r(0, 0))
  }
}
