package starling.titan

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import starling.daterange.Day
import starling.quantity.{UOM, Quantity, Percentage}
import starling.db.TitanTradeSystem
import starling.instrument.{Trade, TradeID}
import starling.instrument.physical.UnallocatedSalesQuota
import starling.quantity.UOM._
import starling.utils.{StarlingSpec, StarlingTest}

class TitanTradeAttributesTest extends StarlingSpec with ShouldMatchers {

  val tradeAttributes = TitanTradeAttributes("quota1", Quantity(1, UOM.MT), "trade id", "gc", "comment", Day(2011, 1, 1), "shape", "finall", Percentage.EPSILON, Percentage.EPSILON, "id1")
  val titanTestTrade = Trade(
        TradeID(1, TitanTradeSystem), Day(2012, 01, 16), "counterparty a", tradeAttributes,
        UnallocatedSalesQuota.sample, Nil)

  "should be equal with different event ids" in {
    val b = tradeAttributes.copy(eventID = "id2")
    tradeAttributes should be === b
    tradeAttributes.hashCode() should be === b.hashCode()
  }

  "should NOT be equal otherwise" in {
    val tradeAttributesB = tradeAttributes.copy(quotaID = "quota2")
    tradeAttributes should not be === (tradeAttributesB)
  }

  "Titan Trade should be equal with equal trade fields and attributes" in {
    val tradeA = titanTestTrade
    val tradeB = titanTestTrade.copy(
      attributes = tradeAttributes.copy(eventID = "id2")
    )

    tradeA should be === tradeB
    tradeA.hashCode() should be === tradeB.hashCode()
  }

  "Titan Trade should not be equal with equal with different tradeable values" in {

    val tradeA = titanTestTrade
    val tradeB = titanTestTrade.copy(
      tradeable = UnallocatedSalesQuota.sample.copy(
        quantity = Quantity(101, MT)
      )
    )

    tradeA should not be === (tradeB)
  }
}
