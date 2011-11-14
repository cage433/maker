package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.{Test, DataProvider}
import org.testng.Assert._
import starling.quantity.UOM._
import starling.market.{Market, TestMarketTest}
import collection.immutable.TreeMap
import starling.marketdata._
import starling.quantity.{Percentage, Quantity}
import starling.daterange._
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.models.{Call, BlackScholes}
import starling.maths.{LeastSquaresFit, BisectSolve}
import org.scalatest.testng.TestNGSuite

class SpreadStdDevsTests extends TestMarketTest with TestNGSuite {

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

    val env = UnitTestingEnvironment(DayAndTime(marketDate, TimeOfDay.EndOfDay), {
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
        case DiscountRateKey(_, d, _) => new Quantity(scala.math.exp(-riskFreeRate * d.daysSinceInYears(marketDate)))
        case _: OilAtmVolAtomicDatumKey => oilVol
      }
    })

    assertEquals(
      env.spreadStdDev(Market.NYMEX_WTI, spread, observationDate, Quantity(strike, USD / BBL)).value,
      expectedValue,
      1e-6
    )
  }

  @Test
  def testBuilder() {
    val builder = new SpreadStdDevSurfaceDataBuilder
    val period1: SpreadPeriod = SpreadPeriod(Month(2011, 1), Month(2011, 2))
    val period3: SpreadPeriod = SpreadPeriod(Month(2011, 3), Month(2011, 4))
    val period2: SpreadPeriod = SpreadPeriod(Month(2011, 2), Month(2011, 3))
    builder.addAtm(period2, new Quantity(2))
    builder.addAtm(period1, new Quantity(2))
    builder.addAtm(period3, new Quantity(3))
    val data = builder.build

    assertEquals(List(period1, period2, period3), data.periods.toList)

  }
}
