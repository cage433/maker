package starling.utils


import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.UOM._
import ScalaTestUtils._

class TestSummingMap{
  @Test
  def test{
    var map = SummingMap.empty[Int]
    map += 1 -> Quantity(10, USD)
    map += 2 -> Quantity(12, EUR)
    map += 1 -> Quantity(20, USD)
    assertEquals(map(1), Quantity(30, USD))
    assertEquals(map(2), Quantity(12, EUR))
  }
}
