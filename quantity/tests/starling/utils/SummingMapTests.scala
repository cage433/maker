package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.quantity.Quantity
import org.testng.Assert
import collection.immutable.SortedMap
import starling.quantity.utils.SummingMap


class SummingMapTests extends TestNGSuite {
  @Test
  def test{
    val map1 = new SummingMap(Map(1 -> new Quantity(10),  2 -> new Quantity(20)))
    val map2 = new SummingMap(Map(2 -> new Quantity(30),  3 -> new Quantity(200)))
    val expected = new SummingMap(Map(1 -> new Quantity(10),  2 -> new Quantity(50), 3 -> new Quantity(200)))
    Assert.assertEquals(expected, map1 ++ map2)
  }

  @Test
  def testWithSortedMap{
    val map = new SummingMap(SortedMap(1 -> new Quantity(10)))
    val map2 = map ++ Map(1 -> new Quantity(5), 2 -> new Quantity(20))
    Assert.assertTrue(map.underlying.isInstanceOf[SortedMap[Int, Quantity]])
    Assert.assertEquals(map2.underlying, Map(1 -> new Quantity(15), 2 -> new Quantity(20)))
  }
}
