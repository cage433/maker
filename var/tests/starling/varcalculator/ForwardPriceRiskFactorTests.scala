package starling.varcalculator

import starling.utils.StarlingTest
import starling.daterange.{DateRange, Month, Day}
import org.testng.annotations.Test
import starling.quantity.UOM._
import org.testng.Assert._
import collection.immutable.TreeSet
import starling.market.{TestMarketSpec, FuturesMarket, Market}

class ForwardPriceRiskFactorTests extends TestMarketSpec{

  @Test
  def testOffset {
    val market = Market.ICE_BRENT
    val marketDay = Day(2010, 4, 9)
    val deliveryMonth = Month(2010, 5)
    val rf = market.priceRiskFactor(marketDay.endOfDay, deliveryMonth).toList match {
      case f::Nil => f match { case a: ForwardPriceRiskFactor => a; case _ => throw new Exception("Expected a ForwardPriceRiskFactor")}
      case _ => throw new Exception("Expected a List")
    }
    assertEquals (ForwardPriceRiskFactor(market, 0, 0), rf)
    assertEquals(rf.period(marketDay.endOfDay).firstMonth, deliveryMonth)
  }

  @Test
  def testSetBehaviour(){
    val market1 = Market.testMarket("fred", USD, MT)
    val market2 = Market.testMarket("ginger", USD, MT)


    val rf1 = ForwardPriceRiskFactor(market1, 180, 180)
    val rf2 = ForwardPriceRiskFactor(market2, 180, 180)

    val set = Set.empty + rf1 + rf2
    assertEquals(2, set.size)
  }

  @Test
  def testOnExpiry {
    val md = Day(2010, 9, 15)
    val market = Market.ICE_BRENT
    val apr = Month(2011, 4)
    market.priceRiskFactor(md.endOfDay, apr) match {
      case Some(rf: ForwardPriceRiskFactor) => {
        assertEquals(rf.period(md.endOfDay), apr)
        assertEquals(market.nthPeriod(md.endOfDay, rf.nPeriodsToEnd), apr)
      }
      case _ => fail("Expected Some")
    }

  }
}
