package starling.marketdata

import org.scalatest.testng.TestNGSuite
import starling.market.Level
import org.testng.annotations.Test
import starling.pivot.MarketValue
import starling.quantity.{UOM, Quantity}
import starling.quantity.Quantity._
import starling.daterange.{Day, Tenor, Month, StoredFixingPeriod}

class PriceFixingsHistoryDataTests extends TestNGSuite {

  @Test def differentDataShouldNotBeEqual {
    val a = PriceFixingsHistoryData.create(Map(
      (Level.Bid, StoredFixingPeriod.tenor(Tenor(Month, 0))) → MarketValue.quantity(10 (UOM.USD)),
      (Level.Ask, StoredFixingPeriod.tenor(Tenor(Month, 0))) → MarketValue.quantity(10 (UOM.USD))
    ))
    val b = PriceFixingsHistoryData.create(Map(
      (Level.Bid, StoredFixingPeriod.tenor(Tenor(Month, 3))) → MarketValue.quantity(10 (UOM.USD)),
      (Level.Ask, StoredFixingPeriod.tenor(Tenor(Month, 3))) → MarketValue.quantity(10 (UOM.USD))
    ))

    assert(a != b)
  }
}