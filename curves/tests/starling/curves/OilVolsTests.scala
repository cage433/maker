package starling.curves

import starling.utils.StarlingTest
import org.testng.Assert._
import starling.quantity.{Quantity, Percentage}
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.daterange.{DateRange, Month, Day, ObservationPoint}
import starling.calendar.{NullHolidays, HolidayTablesFactory}
import starling.marketdata._
import collection.immutable.TreeMap
import org.testng.annotations.{BeforeClass,AfterClass, AfterMethod, BeforeMethod, Test}
import starling.market._
import starling.models.{Call, BlackScholes}
import org.testng.annotations.{AfterClass, DataProvider, Test}

class OilVolsTests extends JonTestEnv {

  val market = Market.NYMEX_WTI

  @Test
  def testUsingEnvironment() {
    import JonTestData._

    val dec10 = Month(2010, 12)
    val jan11 = Month(2011, 1)
    val feb11 = Month(2011, 2)

    val md = Day(2010, 1, 5).endOfDay
    val EPSILON_PRICE = Quantity(0.25, USD / BBL)

    val env = makeEnv(md)

    val T = Day(2010, 6, 17).endOfDay.timeSince(md)
    val atmDelta = new BlackScholes(100, 100, Call, T, 0.37).analyticDelta * math.exp(-0.0033195274725275*T)
    assertEquals(atmDelta, 0.54838180872184, 1e-12)

    val exDaydec10 = market.optionExpiry(dec10)
    val exDayJan11 = market.optionExpiry(jan11)
    val exDayFeb11 = market.optionExpiry(feb11)

    val acAtmVol1 = env.impliedVol(market, dec10, exDaydec10, wtiForward(dec10))
    assertEquals(acAtmVol1, wtiATMVols(dec10), 1e-10)
    val acAtmVol2 = env.impliedVol(market, jan11, exDayJan11, wtiForward(jan11))
    assertEquals(acAtmVol2, wtiATMVols(jan11), 1e-10)
    val acAtmVol3 = env.impliedVol(market, feb11, exDayFeb11, wtiForward(feb11))
    assertEquals(acAtmVol3, wtiATMVols(feb11), 1e-10)

    assertEquals(env.impliedVol(market, dec10, exDaydec10, Quantity(82.5, market.priceUOM)).value, 0.362940564615, 1e-8)
    assertEquals(env.parallelShiftPrices(market, EPSILON_PRICE).impliedVol(market, dec10, exDaydec10, Quantity(82.5, market.priceUOM)).value, 0.363517590157, 1e-8)
    assertEquals(env.parallelShiftPrices(market, -EPSILON_PRICE).impliedVol(market, dec10, exDaydec10, Quantity(82.5, market.priceUOM)).value, 0.362366283705, 1e-8)

    assertEquals(env.impliedVol(market, jan11, exDayJan11, Quantity(82.5, market.priceUOM)).value, 0.359136592145, 1e-8)
    assertEquals(env.parallelShiftPrices(market, EPSILON_PRICE).impliedVol(market, jan11, exDayJan11, Quantity(82.5, market.priceUOM)).value, 0.359696235598, 1e-8)
    assertEquals(env.parallelShiftPrices(market, -EPSILON_PRICE).impliedVol(market, jan11, exDayJan11, Quantity(82.5, market.priceUOM)).value, 0.358579413320, 1e-8)
  }

  @DataProvider(name = "zeroSkewProvider")
  def zeroSkewProvider = {
    Array(
      Array[Object](
        Array[DateRange](Month(2011, 1)),
        Array(Percentage(0.5)),
        Array(0.5)
        )
      ,Array[Object](
        Array[DateRange](Month(2011, 1), Month(2011, 3)),
        Array(Percentage(0.5), Percentage(0.4)),
        Array(0.5, 0.7)
        )
      )
  }
  @Test(dataProvider = "zeroSkewProvider")
  def testZeroSkewHasAtmVol(periods : Array[DateRange], atmVols : Array[Percentage], skewDeltas : Array[Double]){
    val skews : Array[Array[Percentage]] = skewDeltas.map{d => periods.map{p => Percentage(0)}}
    val volSurfaceData : OilVolSurfaceData = OilVolSurfaceData(periods, atmVols, skewDeltas, skews)

    val marketDay = Day(2010, 1, 1)
    val atomicEnv = new MarketDataCurveObjectEnvironment(
      marketDay.endOfDay,
      new MarketDataSlice {
        def read(key:MarketDataKey) = (key match {
          case o : OilVolSurfaceDataKey => volSurfaceData
          case p : PriceDataKey => PriceData.create(periods.map(_ -> 100.0), market.priceUOM)
        })
        def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint) = throw new Exception("Not implemented")
      }, false, ReferenceDataLookup.Null
    )
    val env = Environment(atomicEnv).undiscounted
    atmVols.zip(periods).foreach {
      case (v, p) =>
        assertEquals(
          env.impliedVol(Market.NYMEX_WTI, p, Day(2010, 12, 31), Quantity(123, USD / MT)).value,
          v.decimalValue,
          1e-6
          )
    }

  }
}
