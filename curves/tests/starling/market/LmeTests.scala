package starling.market

import starling.utils.StarlingTest
import org.testng.annotations.{DataProvider, Test}
import org.testng.Assert._
import starling.daterange.{Year, Day}


class LmeTests extends StarlingTest{

  val lme = FuturesExchangeFactory.LME

  @DataProvider(name = "threeMonthDataProvider")
  def threeMonthDataProvider = Array(
    Array(Day(2011, 6, 9), Day(2011, 9, 9))
    ,Array(Day(2011, 6, 10), Day(2011, 9, 9))   // 3 months falls on Saturday so roll back
    ,Array(Day(2010, 11, 30), Day(2011, 2, 28))   // 30th Feb doesn't exist so roll back
    ,Array(Day(2010, 11, 27), Day(2011, 2, 28))   // 27th Feb is a Sunday so roll forward
    ,Array(Day(2011, 7, 1), Day(2011, 10, 3))   // 1st Oct is a Saturday so roll forward to avoid changing month
  )
  @Test(dataProvider="threeMonthDataProvider")
  def testThreeMonthRule(marketDay : Day, expectedThreeMonthDay : Day){
    assertEquals(lme.threeMonthDate(marketDay), expectedThreeMonthDay)
  }

  @Test
  def testThreeMonthDayIsAlwyasInCorrectMonth{
    val cal = Market.cals.LME
    Year(2011).days.filter(cal.isBusinessDay).foreach{
      d =>
        val threeMonths = lme.threeMonthDate(d)
        assertEquals(d.containingMonth + 3, threeMonths.containingMonth)
        assertTrue(cal.isBusinessDay(threeMonths))
    }
  }
}