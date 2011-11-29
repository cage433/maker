package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.market.Market._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.utils.StarlingTest
import starling.market.FuturesSpreadMarket
import starling.curves.Environment
import starling.curves.UnitTestingEnvironment
import starling.daterange.Day
import starling.daterange.Month
import starling.curves.ForwardPriceKey

class FuturesCommoditySpreadTests extends StarlingTest {

  @Test
  def testExplanation{
    val spread = FuturesCommoditySpread(FuturesSpreadMarket.RBHO, Month(2012, 1), Quantity(88, USD/GAL), Quantity(99, USD/GAL), Quantity(1000, GAL))
    val env = UnitTestingEnvironment(
      Day(2011, 8, 2).endOfDay,
      {
        case ForwardPriceKey(NYMEX_GASOLINE, _, _) => Quantity(200, USD/GAL)
        case ForwardPriceKey(NYMEX_HEATING, _, _) => Quantity(180, USD/GAL)
      }
    )
    val explanation = spread.explanation(env)
    assertEquals(explanation.name, "(Future 1 + Future 2)")
    assertEquals(explanation.format(1), "(((F - K) × Volume) + ((F - K) × Volume))")
    assertEquals(explanation.format(2), "(((NYMEX RBOB.JAN 2012 - 88.00 USD/gal) × 1,000.00 gal) + ((NYMEX Heat.JAN 2012 - 99.00 USD/gal) × (1,000.00) gal))")
    val lastExplanation = "(((200.00 USD/gal - 88.00 USD/gal) × 1,000.00 gal) + ((180.00 USD/gal - 99.00 USD/gal) × (1,000.00) gal))"
    assertEquals(explanation.format(3), lastExplanation)
    assertEquals(explanation.format(4), lastExplanation)
  }
}
