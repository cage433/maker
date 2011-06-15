package starling.edm

import starling.utils.StarlingTest
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.quantity.UOM._
import org.testng.annotations.{DataProvider, Test}
import starling.utils.ScalaTestUtils._
import org.testng.Assert._

class EDMConversionsTests extends StarlingTest{

  @DataProvider(name = "testConversionRoundTripProvider")
  def testConversionRoundTripProvider = constructDataProviderArgs(
    List(100 (USD), 10(GBP), 8(GBP/MT), 5(GBP/MT^3), 2((LB * GBP^2)/(MT * CNY)))
  )
  @Test(dataProvider = "testConversionRoundTripProvider")
  def testConversionRoundTrip(q : Quantity){
    import starling.edm.EDMConversions._
     assertEquals(q, fromEDMQuantity(toEDMQuantity(q)))
  }
}