package starling.instrument

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.curves._
import starling.daterange.{DayAndTime, TimeOfDay, Day}
import starling.quantity.{UOM, Quantity}

class CashInstrumentTest extends StarlingTest {
  @Test
  def testExplanation() {
    val cashInstrument = CashInstrument(CashInstrumentType.General, Quantity(100.0, UOM.USD), Day(2011, 1, 20), None, None)

    val marketDay = DayAndTime(Day(2010, 1, 1), TimeOfDay.EndOfDay)
    val env = UnitTestingEnvironment(marketDay, {
      key => key match {
        case DiscountRateKey(_, day, _) => {
          val timeBetween = day.endOfDay.timeSince(marketDay)
          new Quantity(math.exp(-0.05 * timeBetween))
        }
      }
    })

    val explan = cashInstrument.explanation(env)

    assertEquals(explan.name, "(Volume × Discount)")
    assertEquals(explan.format(1), "(100.00 USD × USD.20Jan2011)")
    assertEquals(explan.format(2), "(100.00 USD × 0.95)")
    assertEquals(explan.format(3), "(100.00 USD × 0.95)")
  }
}
