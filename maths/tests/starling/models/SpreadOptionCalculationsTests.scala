package starling.models

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.daterange.Day
import starling.maths.NumericalDerivative

class SpreadOptionCalculationsTests extends TestNGSuite {
  val observationDate = Day(2010, 5, 1)
  val T = (Day(2010, 5, 19) - observationDate) / 365.0

  @Test
  def testOutOfTheMoneyJun10Jul10Call {
//    val sn = SpreadNormal(Call, -3.0, -1.0, 0.005, 16.0, T)
    val cso = new SpreadOptionCalculations(Call, -3.0, -1.0, 16.0, 0.005, T)
    assertEquals(cso.price, 0.636145138746, 1e-12)
    assertEquals(cso.analyticUndiscountedDelta, 0.286756369275, 1e-12)
    assertEquals(cso.analyticUndiscountedGamma, 0.095829510706, 1e-12)
  }
  @Test
  def testInOfTheMoneyJun10Jul10PutTest {
    val cso = new SpreadOptionCalculations(Put, -3.0, -1.0, 16.0, 0.005, T)
    assertEquals(cso.price, 2.635652048856, 1e-12)
    assertEquals(cso.analyticUndiscountedDelta, -0.713243630725, 1e-12)
    assertEquals(cso.analyticUndiscountedGamma, 0.095829510706, 1e-12)
  }
  @Test
  def testAnalyticGreeksNumerically{
    val F0 = -3.0
    def cso(F : Double) = new SpreadOptionCalculations(Put, F, -1.0, 16.0, 0.0, T)
    def valuer(F : Double) = cso(F).undiscountedPrice
    val dx = 1e-4
    val numericDelta = new NumericalDerivative(valuer, dx)(F0)
    val analyticDelta = cso(F0).analyticUndiscountedDelta
    assertEquals(analyticDelta, numericDelta, 1e-3)
    val List(dn, mid, up) = List(-dx, 0, dx).map{dp => valuer(F0 + dp)}
    val numericGamma = (up - 2 * mid + dn) / (dx * dx)
    val analyticGamma = cso(F0).analyticUndiscountedGamma
    assertEquals(analyticGamma, numericGamma, 1e-3)
  }
}
