package starling.edm

import org.testng.annotations.{DataProvider, Test}
import starling.utils.ScalaTestUtils._
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.quantity.Quantity
import com.trafigura.services.TitanSerializableQuantity
import org.testng.Assert._

class TitanSerializableQuantityTests {

  @DataProvider(name = "testRoundTripProvider")
  def testRoundTripProvider = constructDataProviderArgs(
    List(1.0(USD), 2.5(USD/MT), 4.5 (JPY/LB))
  )
  @Test(dataProvider = "testRoundTripProvider")
  def testRoundTrip(qty : Quantity){
    val titanSerializableQuantity = EDMConversions.toTitanSerializableQuantity(qty)
    val titanQuantity = TitanSerializableQuantity.toTitanQuantity(titanSerializableQuantity)
    val qty2 = EDMConversions.fromQuantityE(titanQuantity)
    assertEquals(qty, qty2)
  }
}