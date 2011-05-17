package starling.curves.models

import starling.market.JonTestEnv
import starling.daterange.Day
import org.testng.Assert._
import org.testng.annotations.{BeforeMethod, AfterMethod, Test}

class SpreadVolatilitySkewTests extends JonTestEnv {

  @Test(groups = Array("internal"))
  def paramsTest {
    val T = Day(2010, 5, 19).endOfDay.timeSince(Day(2010, 5, 1).endOfDay)
    val skew = new SpreadVolatilitySkew(16.0, 16.0, 0.005, T, 5.0, 0.0, 2)

    assertEquals(skew.polynomialCoefficients(0), 5.0012330287233, 1e-12)
    assertEquals(skew.polynomialCoefficients(1), 10.004932723037, 1e-12)
  }

  @Test(groups = Array("internal"))
  def valuesTest {
    val T = Day(2010, 5, 19).endOfDay.timeSince(Day(2010, 5, 1).endOfDay)
    val skew = new SpreadVolatilitySkew(16.0, 16.0, 0.005, T, 5.0, 0.0, 2)

    val strikes = List(-7.0, -5.0, -3.0, -1.0)
    val fwdPrice = -3.0
    val skews = strikes.map(skew.volatilityByStrike(_, fwdPrice))
    assertEquals(skews(0), 19.2173738004124, 1e-12)
    assertEquals(skews(1), 17.5209466140729, 1e-12)
    assertEquals(skews(2), 16.0, 1e-12)
    assertEquals(skews(3), 15.388510306823, 1e-12)
  }

}
