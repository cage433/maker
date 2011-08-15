package starling.instrument

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._
import starling.market.Market
import starling.curves.Environment
import starling.curves.UnitTestingAtomicEnvironment
import starling.daterange.Month
import starling.daterange.Day
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.curves.ForwardPriceKey

class FuturesCalendarSpreadTests extends StarlingTest{
  @Test
  def testExplanation{
    val spread = FuturesCalendarSpread(Market.COMEX_SILVER, Month(2012, 1), Month(2012, 2), Quantity(101, USD/OZ), Quantity(99, USD/OZ), Quantity(1000, OZ))
    val env = Environment(
      new UnitTestingAtomicEnvironment(
        Day(2011, 8, 2).endOfDay,
        {
          case ForwardPriceKey(_, Month(y, m), _) => Quantity(100.5 + m, USD/OZ)
        }
      )
    )
    val explanation = spread.explanation(env)
    assertEquals(explanation.name, "(Front + Back)")
    assertEquals(explanation.format(1), "(((F - K) * Volume) + ((F - K) * Volume))")
    assertEquals(explanation.format(2), "(((COMEX Silver.JANUARY 2012 - 101.00 USD/oz) * 1,000.00 oz) + ((COMEX Silver.FEBRUARY 2012 - 99.00 USD/oz) * (1,000.00) oz))")
    val lastExplanation = "(((101.50 USD/oz - 101.00 USD/oz) * 1,000.00 oz) + ((102.50 USD/oz - 99.00 USD/oz) * (1,000.00) oz))"
    assertEquals(explanation.format(3), lastExplanation)
    assertEquals(explanation.format(4), lastExplanation)
  }
}

