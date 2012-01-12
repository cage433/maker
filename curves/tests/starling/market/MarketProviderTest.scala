package starling.market

import starling.utils.StarlingTest
import starling.quantity.UOM._
import starling.calendar.NilCalendar
import starling.daterange.Month
import org.scalatest.WordSpec
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations._
import org.testng.Assert._
import scalaz.Scalaz._

class MarketProviderTest extends TestMarketTest {
  @Test
  def testMarketProviderReloading {
    val quoteidA = 1000001
    val quoteidB = 1000002
    val a = new FuturesMarket("market a", Some(1000), MT, USD, NilCalendar,
      Some(quoteidA), Month, new NoFuturesExpiryRules, FuturesExchangeFactory.NYMEX, WTI)

    // just market a
    TestMarketLookup.add(a)
    TestMarketLookup.changed()
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidA), Some(a))
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidB), None)

    // add market b
    val b = new FuturesMarket("market b", Some(1000), MT, USD, NilCalendar,
      Some(quoteidB), Month, new NoFuturesExpiryRules, FuturesExchangeFactory.NYMEX, WTI)
    TestMarketLookup.add(b)

    // don't refresh so shouldn't be visible
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidA), Some(a))
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidB), None)

    // refresh - now visible
    TestMarketLookup.changed()
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidA), Some(a))
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidB), Some(b))

    // remove b
    TestMarketLookup.remove(b)
    TestMarketLookup.changed()

    // change b - check
    val newb = new FuturesMarket("market b", Some(1000), MT, USD, NilCalendar,
      Some(quoteidB), Month, new NoFuturesExpiryRules, FuturesExchangeFactory.NYMEX, Brent)
    TestMarketLookup.add(newb)
    TestMarketLookup.changed()
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidA), Some(a))
    assertEquals(Market.futuresMarketFromQuoteIDOption(quoteidB).fold(_.commodity, null), Brent)

    // put it back the way we found it
    TestMarketLookup.remove(a)
    TestMarketLookup.remove(newb)
    TestMarketLookup.changed()
  }
}