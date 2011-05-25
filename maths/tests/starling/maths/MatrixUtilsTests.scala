package starling.maths

import org.scalatest.testng.TestNGSuite
import cern.colt.matrix.linalg.CholeskyDecomposition


class RandomCorrelationMatrixTester extends TestNGSuite {
  import org.testng.annotations._
  import org.testng.Assert._
  @Test
  def testMatricesAreCorrelationMatrices{
	  for (i <- 1 until 10){
		  val m = RandomCorrelationMatrix(i * 10, 12345)
		  val cd = new CholeskyDecomposition(m)
		  assertTrue(cd.isSymmetricPositiveDefinite)
	  }
  }
}
