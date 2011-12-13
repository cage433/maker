package starling.curves

import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity
import org.scalatest.testng.TestNGSuite
import starling.market._
import starling.daterange.{DayAndTime, Day}


class MarketDayChangeIndexTests extends TestMarketTest with TestNGSuite {

  /**
   * test SHFE VWAP Month index behaviour
   */
  @Test
  def testMarketDayChangesForShfeMonthVwapIndex {

    val marketDay1 = Day(2011, 1, 15).endOfDay
    val market = Market.SHANGHAI_ALUMINUIUM
    val index = ShfeIndices.shfeMonthVwap(market)

    def mkEnv(marketDay : DayAndTime) : Environment = {
      UnitTestingEnvironment(
        marketDay,
        {
          // used when looking for prices of dates in the future, from future prices
          case ForwardPriceKey(_, _ , _) => new Quantity(10, CNY / MT)
          // used when the market day is on or after the published vwap index price
          case IndexFixingKey(index : ShfeVwapMonthIndex, observationDay: Day) => new Quantity(1000, CNY / MT)
          // used when the market day is before the published vwap index price
          case IndexFixingKey(index : FuturesFrontPeriodIndex, observationDay: Day) => new Quantity(100, CNY / MT)
        }
      )
    }

    // start before the month end vwap fixing and check normal price fixings / futures month averaging is used (unweighted price averaging)
    val indexFixings1 = mkEnv(marketDay1).averagePrice(index, marketDay1.containingMonth)

    // should be average of first 15 days fixed at 100 + remaining at forward 10 ~= 50CNY/MT
    assert(indexFixings1.isAlmostEqual(Quantity(52.857142857142854, CNY / MT), 10E-6), "End of Month did not correctly observe the published VWAP index price")


    // move forward to month end and check we use the VWAP level fixing
    val marketDay2 = (marketDay1.day.containingMonth.lastDay).endOfDay
    val indexFixings2 = mkEnv(marketDay2).averagePrice(index, marketDay2.containingMonth)

    // should be published fixing of indexes VWAP month average price = 1000CNY/MT
    assertEquals(indexFixings2, Quantity(1000, CNY / MT), "End of Month did not correctly observe the published VWAP index price")


    // move forward to beyond month end and check we use the VWAP level fixing
    val marketDay3 = ((marketDay1.day.containingMonth + 1).firstDay + 5).endOfDay
    val indexFixings3 = mkEnv(marketDay3).averagePrice(index, marketDay2.containingMonth)

    // should be published fixing of indexes VWAP month average price = 1000CNY/MT
    assertEquals(indexFixings3, Quantity(1000, CNY / MT), "End of next Month did not correctly observe the published VWAP index price")


    // also check a forward state environment observes future prices
    val shiftedEnv = mkEnv(marketDay1).forwardState(marketDay2)

    val indexFixings4 = shiftedEnv.averagePrice(index, marketDay2.containingMonth)

    assertEquals(indexFixings4, Quantity(10, CNY / MT), "Forward state did not correctly observe the forward index price")
  }
}
