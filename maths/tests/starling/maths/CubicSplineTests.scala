package starling.maths

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import math.log

class CubicSplineTests extends TestNGSuite {

  private def makeXS(xLow : Double, xHigh : Double, size : Int) = {
    val dx = (xHigh - xLow) / (size - 1)
    (0 until size).toArray.map(xLow + _ * dx)
  }
  @DataProvider(name = "testSplineIsCloseProvider")
  def testSplineIsCloseProvider = {
    Array(
      Array(makeXS(1.0, 10.0, 20), log(_), 1e-3, Some(1.0), None)
    )
  }

  @Test(dataProvider = "testSplineIsCloseProvider")
  def testSplineIsClose(xs : Array[Double], fn : Double => Double, tol : Double, yp1: Option[Double], ypn : Option[Double]){
    val spline = new CubicSpline(xs, xs.map(fn), yp1, ypn)
    val xs_ = makeXS(xs(0), xs(xs.size - 1), 200)
    for (x0 <- xs_){
      assertEquals(fn(x0), spline(x0), tol)
    }
  }
}
