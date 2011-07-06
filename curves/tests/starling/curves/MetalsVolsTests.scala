package starling.curves

import starling.utils.StarlingTest
import starling.marketdata._
import starling.quantity.{Quantity, Percentage}
import org.testng.annotations.Test
import starling.calendar.{BusinessCalendars, NullHolidays, HolidayTablesFactory}
import org.testng.Assert._
import starling.market._
import starling.daterange.{ObservationPoint, DateRange, Month, Day}

class MetalsVolsTests extends TestMarketSpec {
  @Test
  def test {
    val months = Array[DateRange](Month(2010, 2), Month(2010, 6), Month(2011, 1))
    val vols = Array(0.4, 0.4, 0.5).map(Percentage(_))
    val marketDay = Day(2010, 1, 1)
    val atomicEnv = new MarketDataCurveObjectEnvironment(
      marketDay.endOfDay,
      new MarketDataSlice {
        def read(key:MarketDataKey): MarketData = (key match {
          case BradyMetalVolsDataKey(market) => market.tenor match {
            case Day => BradyMetalVolsData.create(Map() ++ (months zip vols).map{case(m,v)=>((0.5,m.firstDay),v)})
            case Month => BradyMetalVolsData.create(Map() ++ (months zip vols).map{case(m,v)=>((0.5,m),v)})
          }
        })

        def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint) = throw new Exception("Not implemented")
      }
      )
    val env = Environment(atomicEnv)

    months.zip(vols).foreach{
      case (m, v) =>
        assertEquals(
          env.impliedVol(Market.COMEX_GOLD, m, m.firstDay - 1, Quantity(50, Market.COMEX_GOLD.priceUOM)).decimalValue,
          v.decimalValue,
          1e-9
        )
        assertEquals(
          env.impliedVol(Market.LME_LEAD, m.firstDay, m.firstDay - 1, Quantity(50, Market.LME_LEAD.priceUOM)).decimalValue,
          v.decimalValue,
          1e-9
        )
    }
  }

  @Test
  def testInterpolation {
    val vols:Map[(Double, DateRange), Percentage] = Map( (11.0, Month(2009, 10)) -> Percentage(.36), (11.0, Month(2013, 4)) -> Percentage(.40), (11.0, Month(2011, 4)) -> Percentage(.40), (11.0, Month(2009, 5)) -> Percentage(.35), (11.0, Month(2009, 6)) -> Percentage(.36), (11.0, Month(2009, 7)) -> Percentage(.36), (11.0, Month(2009, 4)) -> Percentage(.35), (11.0, Month(2010, 4)) -> Percentage(.38), (11.0, Month(2012, 4)) -> Percentage(.40)
      )
    val vol = new MetalsVols(null, Market.COMEX_PLATINUM, BradyMetalVolsData.create(vols)).apply(Month(2010, 12))
    assertEquals(vol.decimalValue, .393333, 1e-5)
  }
}
