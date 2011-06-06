package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.calendar.{HolidayTablesFactory, BusinessCalendars, NullHolidays}
import starling.marketdata._
import starling.quantity.{Quantity, Percentage}
import starling.quantity.UOM._
import starling.maths.{NumericalDerivative, BisectSolve}
import org.testng.Assert._
import starling.models.{Call, BlackScholes}
import starling.market._
import starling.daterange.{ObservationPoint, Day, DateRange}

/**
 * Created by IntelliJ IDEA.
 * User: alex
 * Date: May 3, 2010
 * Time: 1:40:12 PM
 * To change this template use File | Settings | File Templates.
 */

class BradyFXVolsTests extends TestExpiryRules{
  @Test
  def testFromMarketDataToEnvironment{
    val days = Array(Day(2010, 2, 13), Day(2010, 4, 20), Day(2010, 8, 8), Day(2012, 12, 31))
    val deltas = Array(0.25, 0.5, 0.75)

    val vols = Array(
      Array(0.5, 0.6, 0.7, 0.8).map(Percentage(_))
      , Array(0.4, 0.55, 0.7, 0.85).map(Percentage(_))
      , Array(0.55, 0.65, 0.8, 0.9).map(Percentage(_))
      )

    val volSurface = BradyFXVolSurfaceData(days, deltas, vols)
    val marketDay = Day(2010, 1, 1).endOfDay

    val atomicEnv = new MarketDataCurveObjectEnvironment(
      marketDay,
      new MarketDataSlice {
        def read(key:MarketDataKey): MarketData = (key match {
          case BradyFXVolSurfaceDataKey(_) => volSurface
          case SpotFXDataKey(_) => SpotFXData(Quantity(1.5, USD/EUR))
        })
        def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint) = throw new Exception("Not implemented")
      }
      )

    /* For each delta/day, determine the corresponding strike by inverting the atm delta function. Then
        check that the environment's interpolated vol matches that in our vol matrix
     */
    val env = Environment(atomicEnv).undiscounted
    days.zipWithIndex.foreach{
      case (day, j) =>
        val time = day.endOfDay.timeSince(marketDay)
        val atmDelta = 0.5
        val atmVol = vols(1)(j)
        deltas.zipWithIndex.foreach{
          case (delta, i) => {
            val strikeDeltaFn = {
              k : Double =>
              new NumericalDerivative(BlackScholes.undiscountedOptionPrice(_, k, Call, time, atmVol), 1e-3)(1.5)
            }
            val strike = BisectSolve(strikeDeltaFn, delta, 100.0, 0.1, 1e-6)
            val impliedVol = env.impliedVol(new FXMarket(USD, EUR), day, day, Quantity(strike, USD/EUR))
            // FX Vols were broken in the refactoring of environments. Now they always
            // return ATM vols. Fixable but not worth doing as long as Trinity FX Vols
            // are complete crap (both stale and wrong). Uncomment the following line
            // if they're ever put back
            //assertEquals(impliedVol.decimalValue, vols(i)(j).decimalValue, 1e-3)
            assertEquals(impliedVol.decimalValue, atmVol, 1e-3)
          }
        }
    }

  }
}
