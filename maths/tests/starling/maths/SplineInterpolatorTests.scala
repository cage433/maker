package starling.maths

import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations.{DataProvider, Test}
import math._


class SplineInterpolatorTests extends TestNGSuite {
  @DataProvider(name = "testInterpolationGivesValuesCloseToFunctionProvder")
  def testInterpolationGivesValuesCloseToFunctionProvder = {
    Array(
      Array(sin _, 0.0, 2.0, 20, 100, 1e-2)
      ,Array({x : Double => log(1 + x) * cos(x)}, 0.0, 4.0, 20, 100, 1e-2)

      )
  }
  @Test(dataProvider = "testInterpolationGivesValuesCloseToFunctionProvder")
  def testInterpolationGivesValuesCloseToFunction(
    fn : Double => Double, xLow : Double, xHigh : Double, nFnSteps : Int, nTestSteps : Int, epsilon : Double
  )
  {
    def grid(n : Int) = {
      val dx = (xHigh - xLow) / (n - 1)
      (0 to n + 1).toArray.map(xLow + _ * dx)
    }

    val xs = grid(nFnSteps)
    val ys = xs.map(fn)
    def interp(x : Double) = SplineInterpolator.interpolate(xs, ys, x)
    grid(nTestSteps).foreach{
      x =>
        assertEquals(fn(x), interp(x), epsilon)
    }
  }
}
