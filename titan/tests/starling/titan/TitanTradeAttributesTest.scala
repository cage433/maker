package starling.titan

import starling.utils.StarlingTest
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import starling.daterange.Day
import starling.quantity.{UOM, Quantity, Percentage}

class TitanTradeAttributesTest extends WordSpec with ShouldMatchers  {

  "should be equal with different event ids" in {
    val a = TitanTradeAttributes("qote", Quantity(1, UOM.MT), "trade id", None, "gc", "comment", Day(2011, 1, 1), "shape", "finall", Percentage.EPSILON, Percentage.EPSILON, "id1")
    val b = TitanTradeAttributes("qote", Quantity(1, UOM.MT), "trade id", None, "gc", "comment", Day(2011, 1, 1), "shape", "finall", Percentage.EPSILON, Percentage.EPSILON, "id2")
    a should be === b
    a.hashCode() should be === b.hashCode()
  }

  "should NOT be equal otherwise" in {
    val a = TitanTradeAttributes("qote2", Quantity(1, UOM.MT), "trade id", None, "gc", "comment", Day(2011, 1, 1), "shape", "finall", Percentage.EPSILON, Percentage.EPSILON, "id1")
    val b = TitanTradeAttributes("qote3", Quantity(1, UOM.MT), "trade id", None, "gc", "comment", Day(2011, 1, 1), "shape", "finall", Percentage.EPSILON, Percentage.EPSILON, "id1")
    a should not be === (b)
  }

}