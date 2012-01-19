package starling.marketdata

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.quantity.{Quantity, UOM}
import starling.pivot.Field

class MarketDataRowsTest extends WordSpec with ShouldMatchers {
  "can convert to market data" in {
    val rate = Quantity(1.234, UOM.GBP / UOM.USD)

    MarketDataRows(List(Map(Field("Rate") → rate))).toMarketData(SpotFXDataType) should be === SpotFXData(rate)
  }
}