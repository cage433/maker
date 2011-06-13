package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.{Test, DataProvider}
import org.testng.Assert._
import starling.quantity.UOM._
import starling.market.{Market, TestMarketSpec}
import collection.immutable.TreeMap
import starling.marketdata._
import starling.quantity.{Percentage, Quantity}
import starling.daterange._
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.models.{Call, BlackScholes}
import starling.maths.{LeastSquaresFit, BisectSolve}

class SpreadStdDevsTests extends TestMarketSpec {

  @DataProvider(name = "zeroSkewProvider")
  def zeroSkewProvider = {
    Array(
      Array[Object](double2Double(-7.0), double2Double(19.2173738004124)),
      Array[Object](double2Double(-5.0), double2Double(17.5209466140729)),
      Array[Object](double2Double(-3.0), double2Double(16.0)),
      Array[Object](double2Double(-1.0), double2Double(15.388510306823))
    )
  }

  @Test
  def testPolynomialFitIsTheSameAsJons {
    val marketDate = Day(2010, 5, 1)
    val observationDate = Day(2010, 5, 19) // third wednesday
    val time = observationDate.daysSinceInYears(marketDate)
    val riskFreeRate = 0.005
    val df = scala.math.exp(-riskFreeRate * time)

    val coeffs = LeastSquaresFit.fitPolynomialCoefficientsWithZero(2, 0.5 * df, Map(0.0 -> 0.0, df -> 5.0))

    assertEquals(coeffs(0), 5.0012330287233, 1.0e-10)
    assertEquals(coeffs(1), 10.004932723037, 1.0e-10)
  }

  @Test(dataProvider = "zeroSkewProvider")
  def testZeroSkewHasAtmVol(
                             strike: Double,
                             expectedValue: Double) {
    val atmSD = Quantity(16.0, USD / BBL)
    val marketDate = Day(2010, 5, 1)
    val observationDate = Day(2010, 5, 19) // third wednesday
    val time = observationDate.daysSinceInYears(marketDate)
    val riskFreeRate = 0.005
    val forwardPrice = Quantity(-3.0, USD / BBL)
    val oilVol = Percentage(0.16)
    val month = Month(2010, 6)
    val spread = Spread(month, month.next)

    val atomicEnv = new UnitTestingAtomicEnvironment(DayAndTime(marketDate, TimeOfDay.EndOfDay), {
      key => key match {
        case _: SpreadAtmStdDevAtomicDatumKey => atmSD
        case _: SpreadSkewStdDevAtomicDatumKey => {
          val matrix = new DenseDoubleMatrix2D(1, 2)
          matrix.set(0, 0, 0.0);
          matrix.set(0, 1, 5.0)
          matrix
        }
        case ForwardPriceKey(_, spread.first, _) => forwardPrice
        case ForwardPriceKey(_, spread.last, _) => forwardPrice.copy(value = 0.0)
        case DiscountRateKey(_, d, _) => scala.math.exp(-riskFreeRate * d.daysSinceInYears(marketDate))
        case _: OilAtmVolAtomicDatumKey => oilVol
      }
    })

    val env = Environment(atomicEnv)

    assertEquals(
      env.spreadStdDev(Market.NYMEX_WTI, spread, observationDate, Quantity(strike, USD / BBL)).value,
      expectedValue,
      1e-6
    )
  }
}
