package starling.curves

import starling.utils.StarlingTest
import starling.daterange.Day
import org.testng.annotations._
import starling.curves.TestEnvironmentBuilder._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.utils.ScalaTestUtils._
import starling.utils.QuantityTestUtils._

class ForwardFXRateTests extends StarlingTest {

  @Test
  def testWeDidntInvertTheDiscountsInTheForwardFXCalc{
    val marketDay = Day(2009, 1, 1).endOfDay
    val maturityDate = Day(2010, 4, 13)
    
    val usdPerEUR = new Quantity(0.75, USD / EUR) 
    val eurZeroRate = 0.05
    val usdZeroRate = 0.1

    val envBldr = TestEnvironmentBuilder(marketDay)
    envBldr.setUSDPerCCYSpotRate(EUR, usdPerEUR)
    envBldr.setZeroRate(USD, usdZeroRate)
    envBldr.setZeroRate(EUR, eurZeroRate)
    val env = envBldr.build
    
    val forwardEurPerUsd = env.forwardFXRate(EUR, USD, maturityDate)
    
    // now do the arb
    val originalUSD = Quantity(1.0, USD)
    val originalEUR = originalUSD / usdPerEUR
    val forwardUSD = originalUSD / env.discount(USD, maturityDate)
    val forwardEUR = originalEUR / env.discount(EUR, maturityDate)
    val arbedForwardFX = forwardEUR / forwardUSD
    
    assertQtyEquals(arbedForwardFX, forwardEurPerUsd, 1e-6)
  }
}
