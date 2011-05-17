package starling.utils.conversions

import cern.colt.function.{ DoubleDoubleFunction, DoubleDoubleProcedure, DoubleFunction, DoubleProcedure }
import cern.colt.list.DoubleArrayList
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix, DenseDoubleMatrix3D => DTensor}
import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix, DoubleMatrix3D => Tensor}
import cern.jet.math.Functions
import java.lang.Math


/**
 * Adds matrix operators (in prticular +, -, *) to the Colt matrix classes. Enables one to write
 * things like
 * 	A + B
 * rather than
 * 	A.copy.assign(B, Functions.plus)
 */
object RichColtMatrices {

	implicit def fn2DoubleFunction(f : Double => Double) = new DoubleFunction {
		override def apply(a : Double) = f(a)
	}

	implicit def fn2DoubleDoubleFunction(f : (Double, Double) => Double) = new DoubleDoubleFunction {
		override def apply(a : Double, b : Double) = f(a, b)
	}

	implicit def fn2DoubleProcedure(f : Double => Unit) = new DoubleProcedure {
		override def apply(a : Double) = { f(a) ; true }
	}

	implicit def fn2DoubleProcedure2(f : Double => Boolean) = new DoubleProcedure {
		override def apply(a : Double) = f(a)
	}

	implicit def fn2DoubleDoubleProcedure(f : (Double, Double) => Unit) = new DoubleDoubleProcedure {
		override def apply(a : Double, b : Double) = { f(a, b) ; true }
	}

	implicit def fn2DoubleProcedure2(f : (Double, Double) => Boolean) = new DoubleDoubleProcedure {
		override def apply(a : Double, b : Double) = f(a, b)
	}

	object DoubleArrayList {
		def apply(es : Double*) = new DoubleArrayList(es.toArray)
	}

 implicit def DM3D3RichDM3D(m : Tensor) = new RichTensor(m)
	class RichTensor(m : Tensor) {
	  def dimensions = (m.slices, m.rows, m.columns)
		def apply(iSlice : Int) : Matrix = m.viewSlice(iSlice)
	}


	implicit def DM2D2RichDM2D(m : Matrix) = new RichMatrix(m)
	class RichMatrix(m : Matrix) {
		object Algebra extends cern.colt.matrix.linalg.Algebra
		import Algebra._

		def *=(a : Double) = m.assign(new DoubleFunction { override def apply(xi : Double) = xi*a })
		def /=(a : Double) = m.assign(new DoubleFunction { override def apply(xi : Double) = xi/a })
		def +=(a : Double) = m.assign(new DoubleFunction { override def apply(xi : Double) = xi+a })
		def -=(a : Double) = m.assign(new DoubleFunction { override def apply(xi : Double) = xi-a })

		def :=(x : Matrix) = m.assign(x)
		def *(x : Matrix) = mult(m, x)
    def +(x : Matrix) = m.copy.assign(x, Functions.plus)
		def *(x : Vector) = mult(m, x)
		def -(x : Matrix) = m.copy.assign(x, Functions.minus)
		def *(x : Double) = m.copy.assign(Functions.mult(x))
		def /(x : Double) = m.copy.assign(Functions.div(x))
		def -=(x : Matrix) = m.assign(x, Functions.minus)
		def +=(x : Matrix) = m.assign(x, Functions.plus)
		def update(i : Int, j : Int, x : Double) = m.set(i, j, x)
		def vCol(iCol : Int) : Vector = m.viewColumn(iCol)
		def vRow(iRow : Int) : Vector = m.viewRow(iRow)
		def col(iCol : Int) : Vector = m.viewColumn(iCol).copy
		def row(iRow : Int) : Vector = m.viewRow(iRow).copy
		def apply(iRow : Int) : Vector = m.viewRow(iRow)
		def apply(iRow : Int, iCol : Int) : Double = m.get(iRow, iCol)

    /**
     * a one-dimensional column packed copy of the internal array.
     */
    def columnPackedCopy: Vector = {
      val v = new DVector(m.columns * m.rows)
      for(i <- 0 until m.rows; j <- 0 until m.columns) {
        v.set(i + j * m.rows, m.get(i, j))
      }
      v
    }

		def viewColumns() : Iterable[Vector] = new Iterable[Vector]{
			override def iterator() : Iterator[Vector] = new Iterator[Vector]{
				private var i_col = 0;
				override def hasNext : Boolean = {i_col < m.columns}
				override def next : Vector = {
						if (! hasNext )
							throw new NoSuchElementException
							else {
								val v = m.viewColumn(i_col)
								i_col += 1
								v
							}
				}
			}
		}
		def viewRows() : Iterable[Vector] = new Iterable[Vector]{
			override def iterator() : Iterator[Vector] = new Iterator[Vector]{
				private var i_row = 0;
				override def hasNext : Boolean = {i_row < m.rows}
				override def next : Vector = {
						if (! hasNext )
							throw new NoSuchElementException
							else {
								val v = m.viewRow(i_row)
								i_row += 1
								v
							}
				}
			}
		}
	}


 implicit def DM1D2RichDM1D(v : Vector) = new RichVector(v)
 implicit def DoubleList2DM1D(v : List[Double]) = new DVector(v.toArray)

  class RichVector(v : Vector) extends Iterable[Double]{
    def update(i : Int, x : Double) = v.set(i, x)
    override def size : Int = v.size
    override def toString : String = ""
    def toArray = v.toArray
    def zipWithIndex() : List[(Double, Int)] = toList.zipWithIndex
    def |*|(rhs : Vector) : Vector  = {
    	// termwise multiplication
    	v.copy().assign(rhs, Functions.mult)
    }
    def |*=|(rhs : Vector) : Vector  = {
    	// termwise multiplication in place
    	v.assign(rhs, Functions.mult)
    }
    def |/|(rhs : Vector) : Vector  = {
    	// termwise division
    	v.copy().assign(rhs, Functions.div)
    }
    def |/=|(rhs : Vector) : Vector  = {
    	// termwise division in place
    	v.assign(rhs, Functions.div)
    }
    // Assignment
    def := (rhs : Vector) : Vector = {
      v.assign(rhs)
    }
    // Assignment
    def := (rhs : Array[Double]) : Vector = {
      v.assign(new DVector(rhs))
    }
    // Assignment
    def := (rhs : List[Double]) : Vector = {
      v.assign(new DVector(rhs.toArray))
    }
		override def iterator() : Iterator[Double] = new Iterator[Double]{
				private var i_row = 0;
				override def hasNext : Boolean = {i_row < v.size}
				override def next : Double = {
						if (! hasNext )
							throw new NoSuchElementException
							else {
								val x = v(i_row)
								i_row += 1
								x
							}
				}
			}
//    def asIterable : Iterable[Double] = this
    def *(a : Double) = v.copy.assign(Functions.mult(a))
    def /(a : Double) = v.copy.assign(Functions.div(a))
    def +(a : Double) = v.copy.assign(Functions.plus(a))
    def -(a : Double) = v.copy.assign(Functions.minus(a))
    def + (rhs : Vector) : Vector = {
      v.copy().assign(rhs, Functions.plus)
    }
    def - (rhs : Vector) : Vector = {
      v.copy().assign(rhs, Functions.minus)
    }
    def * (rhs : Vector) : Vector = {
      v.copy().assign(rhs, Functions.mult)
    }
    def / (rhs : Vector) : Vector = {
      v.copy().assign(rhs, Functions.div)
    }
    def min (rhs : Vector) : Vector = {
       v.copy().assign(rhs, Functions.min)
     }
    def max (rhs : Vector) : Vector = {
       v.copy().assign(rhs, Functions.max)
     }
    def sqrt() : Vector = {
      v.copy.assign(Functions.sqrt)
    }
    def log() : Vector = {
      v.copy.assign(Functions.log)
    }
    def exp() : Vector = {
      v.copy.assign(Functions.exp)
    }

    def *= (x : Double) : Vector = {
      v.assign(Functions.mult(x))
    }
    def -= (x : Double) : Vector = {
      v.assign(Functions.minus(x))
    }
    def += (rhs: Vector) : Vector = {
      v.assign(rhs, Functions.plus)
    }
    def -= (rhs: Vector) : Vector = {
      v.assign(rhs, Functions.minus)
    }

    def apply(i : Int) = v.getQuick(i)
    def toColumnMatrix : Matrix = {
      val m = new DMatrix(v.size, 1)
      m.viewColumn(0).assign(v.copy)
      m
    }
    def toRowMatrix : Matrix = {
      val m = new DMatrix(1, v.size)
      m.viewRow(0).assign(v.copy)
      m
    }
    override def last : Double = v.get(v.size - 1)
    def mean : Double = {
      v.size match {
        case 0 => 0.0
        case s => v.zSum / s
      }
    }
    def variance : Double = {
      v.size match {
        case 0 => 0.0
        case 1 => 0.0
        case s =>
          val mu = v.mean
          // Handle the case where rounding errors lead to a small negative variance
          Math.abs ( ( (v |*| v) - mu * mu).zSum / s )
      }

    }
    def standardDeviation() : Double = Math.sqrt(variance)
    def expStdDev(weight : Double) : Double = {
      val c = v |*| v
      var sum = 0.0
      for (i <- 0 until v.size)
        sum += c(i) * Math.pow (weight, i)
      Math.sqrt(sum * (1.0 - weight))
    }
    def stdDev() : Double = standardDeviation
    def standardError() = {
      v.size match {
        case 0 => 0.0
        case s => standardDeviation / Math.sqrt(s)
      }

    }
    def correlation(rhs : Vector) : Double = {
      require(v.size == rhs.size, "Mismatching vector size")
      v.size match {
        case 0 => 0.0
        case 1 => 0.0
        case _ if (stdDev * rhs.stdDev == 0) => 0.0
        case _ =>  {
          var rho = covariance(rhs) / (stdDev * rhs.stdDev)
          // Rounding errors may cause this number to be outside [-1, 1]
          rho = Math.max(-1.0, rho)
          rho = Math.min(1.0, rho)
          rho
        }
      }
    }
    def covariance(rhs : Vector) : Double = {
      ( (this - mean) |*| (rhs - rhs.mean) ).mean
    }
    def percentile(percentile : Double) : Double = {
      val orderedValues = v.copy.toArray.toList.sortWith(_>_)
      val index = (v.size * (1.0 - percentile)).asInstanceOf[Int]
      orderedValues(index) - mean
    }

    def asColumnMatrix: Matrix = {
      val m = new DMatrix(v.size, 1)
      m.viewColumn(0).assign(v)
      m
    }

    def asRowMatrix: Matrix = {
      val m = new DMatrix(1, v.size)
      m.viewRow(0).assign(v)
      m
    }

    def map(fn : Double => Double) = v.copy.assign(fn)
  }
}
