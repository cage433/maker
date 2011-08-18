package starling.db

import starling.instrument._
import org.testng.annotations.Test
import starling.pivot.Field
import starling.quantity.UOM._
import starling.market._
import starling.models.Put
import starling.daterange._
import starling.trade.{TradeID, Trade}
import starling.tradestore._
import eai.EAITradeAttributes
import starling.quantity.RichQuantity._
import starling.quantity.Quantity
import org.scalatest.matchers.ShouldMatchers
import MapMatcher._
import starling.eai.TreeID

class TradeableFieldsTest extends TestMarketTest with ShouldMatchers {
  val trade: Trade = Trade(TradeID(1, EAITradeSystem), Day(2009, 1, 1), "cp",
    EAITradeAttributes(TreeID(1), TreeID(2), TreeID(3), "trader", "tradedfor", "broker", "clearinghouse"), ErrorInstrument("error1"))
  val fxOption = FXOption(1.3(EUR/USD), 999(USD), Day(2009, 9, 8), Day(2009, 10, 10), Put)

  @Test
  def strikeFieldShouldBeQuantity {
    TradeableFields.createFieldValues(trade, fxOption) should containEntries[Field, Any] (
      Field("Strike") -> fxOption.strike
    )
  }
}