package starling.maths

import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations.{DataProvider, Test}
import scala.math._
import collection.immutable.TreeMap


class LeastSquaresFitTests extends TestNGSuite {

	@Test
	def testFit1 {
		// An example for Dec-10 WTI from Vas's sheet
		val dataPoints = Map(0.90 -> 0.077, 0.75 -> 0.0375, 0.65 -> 0.0175, 0.45 -> -0.016, 0.30 -> -0.015, 0.1 -> -0.005)
		val coefficients = LeastSquaresFit.fitPolynomialCoefficientsWithZero(3, 0.56293585, dataPoints)
		assertEquals(coefficients(0), 0.14517236, 1e-8)
		assertEquals(coefficients(1), 0.27098797, 1e-8)
		assertEquals(coefficients(2), -0.04986343, 1e-8)
	}

	@Test
	def testFit2 {
		// An example for Dec-11 WTI from Vas's sheet
		val dataPoints = Map(0.90 -> 0.0725, 0.75 -> 0.035, 0.65 -> 0.015, 0.45 -> -0.011, 0.30 -> -0.01, 0.1 -> 0.005)
		val coefficients = LeastSquaresFit.fitPolynomialCoefficientsWithZero(3, 0.57151173, dataPoints)
		assertEquals(coefficients(0), 0.12667279, 1e-8)
		assertEquals(coefficients(1), 0.29662513, 1e-8)
		assertEquals(coefficients(2), 0.00440463, 1e-8)
	}

  @DataProvider(name = "testPolynomialFitProvider")
  def testPolynomialFitProvider = Array(
    Array(List(1.0, 2.0, -3.0))
    ,Array(List(1.0, 0.0, -3.0, 4.0))
    ,Array(List(1.0))
    )

  @Test(dataProvider = "testPolynomialFitProvider")
  def testThatPolynomialsFitExactly(coeffs : List[Double]){
    def poly(x: Double)  = (0.0 /: coeffs.zipWithIndex.map{case (c, i) => c * pow(x, i)})(_+_)

    // Test that polynomials of at least coeffs.size order fit given (x, y)'s exactly
    for (order <- coeffs.size to coeffs.size + 3) {
      val xs = (0 to order).toList.map(3.2 + _ * 0.87)
      val ys = xs.map(poly)
      val fittedPoly = LeastSquaresFit.fitPolynomial(order, TreeMap(xs.zip(ys): _*))

      xs.foreach {
        x =>
          assertEquals(poly(x), fittedPoly(x), 1e-8)
      }

      // test that poly of order coeffs.size matches the given poly exactly
      if (order == coeffs.size){
        (0 to 10).foreach{x =>
          assertEquals(poly(x), fittedPoly(x), 1e-9)
        }
      }
    }


  }
}
