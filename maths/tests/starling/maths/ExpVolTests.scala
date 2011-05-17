package starling.maths


import org.scalatest.testng.TestNGSuite
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import org.testng.annotations.Test
import org.testng.Assert._
import starling.utils.conversions.RichColtMatrices._

class ExpVolTests extends TestNGSuite {
  @Test
  def testExponentialVolDoesntIntroduceBias{
    val seed = 12345
    val N = 100
    val nIterations = 5000
    val mu = 0.0
    val sd = 1.5
    val sn = RandomVariables.normal(mu, sd, 12345)
    val sampleStdDevs : List[Double] = (0 until nIterations).toList.map{
      i =>
        val v = new DVector(N).assign(sn)
        v.expStdDev(0.94)
    }
    val aveStdDev = new DVector(sampleStdDevs.toArray).mean
    assertEquals(aveStdDev, sd, sd * 0.01)
  }
}
