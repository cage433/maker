package starling.quantity

import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.RichQuantity._

class SpreadQuantityTests {

  @Test
  def testParse {
    assertEquals(SpreadQuantity.parse("0.0/-9.9999999999E10 USD/bbl").get, SpreadQuantity(0 (USD/BBL), (-9.9999999999E10) (USD/BBL)))
    assertEquals(SpreadQuantity.parse("-0.0/-9.9999999999E10 USD/bbl").get, SpreadQuantity(0 (USD/BBL), (-9.9999999999E10) (USD/BBL)))
    assertEquals(SpreadQuantity.parse("1.0E-25/-1.0E-25 USD/bbl").get, SpreadQuantity(1.0E-25 (USD/BBL), (-1.0E-25) (USD/BBL)))
  }
}