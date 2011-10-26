package starling.instrument

import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.Quantity._
import starling.quantity.UOM._
import starling.market.{Index, JonTestEnv}
import starling.daterange.{Day, Year, Month, DayAndTime}
import starling.curves._
import starling.quantity.{Percentage, Quantity}

class FFATest extends JonTestEnv {

  val fixings = Map((3, 149.4400),
    (4, 150.0000),
    (5, 150.8300),
    (6, 153.0600),
    (7, 154.4400),
    (10, 156.3900),
    (11, 157.7800),
    (12, 158.8900),
    (13, 158.0600),
    (14, 158.0000),
    (17, 158.0600),
    (18, 158.0600),
    (19, 158.3300),
    (20, 156.1100),
    (21, 156.3300),
    (24, 154.7200),
    (25, 153.3300))

  def makeEnv(md: DayAndTime) = {
    Environment(new UnitTestingAtomicEnvironment(md, {
      key => key match {
        case FreightFlatRateKey(_, _, _) => 5.66 (USD/MT)
        case ForwardPriceKey(_, _, _) => 157.5 (PERCENT)
        case IndexFixingKey(_, Day(2011, 10, d)) => Percentage.fromPercentage(fixings(d)).toQuantity
      }
    })).undiscounted
  }

  @Test
  def testFFA {
    // Test against FF4560169 from Aspect on 25Oct2011
    val V = -12000(MT)
    val fixed = Percentage.fromPercentage(153)
    val rate2011 = 5.66(USD / MT)
    val index = Index.TC6_CROSS_MEDITERRANEAN_30KT_BALTIC
    val period = Month(2011, 10)
    val ffa = new FFA(index, V, fixed, period, Year(2011), Some(rate2011), Year(2011), Some(4))

    val env = makeEnv(Day(2011, 10, 25).endOfDay)
    val mtm = ffa.mtm(env)

    assertQtyEquals(mtm, Quantity(-1903.2, USD), 1e-6)
  }

  @Test
  def testFFAExplain {
    val V = -12000(MT)
    val fixed = Percentage.fromPercentage(153)
    val rate2011 = 5.66(USD / MT)
    val index = Index.TC6_CROSS_MEDITERRANEAN_30KT_BALTIC
    val period = Month(2011, 10)
    val ffa = new FFA(index, V, fixed, period, Year(2011), Some(rate2011), Year(2011), Some(4))

    val env = makeEnv(Day(2011, 10, 25).endOfDay)
    val explanation = ffa.explanation(env)

    assertEquals(explanation.format(0), "((F - K) * V)")
    assertEquals(explanation.format(1), "((Round((Average(TC6 Cross Mediterranean 30KT (Baltic).OCTOBER 2011) * Floating WSC Y2011), 4) - Round((Fixed * Fixed Rate), 4)) * (12,000.00) MT)")
  }
}