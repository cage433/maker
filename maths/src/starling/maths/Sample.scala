package starling.maths


import cern.colt.matrix.{DoubleMatrix2D => Matrix, DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D => DMatrix, DenseDoubleMatrix1D => DVector}
import starling.utils.conversions.RichColtMatrices._

class Sample(val values : Matrix){
  private def v(col : Int) = values.viewColumn(col)
  def mean(col : Int) = v(col).mean
  def stdDev(col : Int) = v(col).standardDeviation
  def mean : Double = {
    assert(values.columns == 1)
    mean(0)
  }
  def stdDev : Double = {
    assert(values.columns == 1)
    stdDev(0)
  }
  def covariance(col1 : Int, col2 : Int) = {
    v(col1).covariance(v(col2))
  }
  def correlation(col1 : Int, col2 : Int) = v(col1).correlation(v(col2))

  def apply(col : Int) = values.viewColumn(col)
}

object Sample{
  def apply(v : Function0[_], n : Int) : Sample = {
    val arrays = (0 until n).toList.map {
      i =>
        v() match {
          case x: Double => Array(x)
          case x: Seq[_] => x.map(_.asInstanceOf[Double]).toArray
          case x : Array[Double] => x
        }
    }
    val cols = arrays(0).length
    val m = new DMatrix(n, cols)
    (0 until n).toList.zip(arrays).foreach{
      case (i, v) =>
        m(i).assign(v)
    }
    new Sample(m)
  }

}