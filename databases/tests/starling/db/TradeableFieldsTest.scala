package starling.db

import starling.instrument._
import org.testng.annotations.Test
import starling.pivot.Field
import starling.quantity.UOM._
import starling.market._
import starling.daterange._
import starling.instrument.{TradeID, Trade}
import starling.tradestore._
import eai.EAITradeAttributes
import starling.quantity.RichQuantity._
import starling.quantity.Quantity
import org.scalatest.matchers.ShouldMatchers
import MapMatcher._
import starling.eai.TreeID
import starling.models.{European, Call, Put}

class TradeableFieldsTest extends TestMarketTest with ShouldMatchers {
  val trade: Trade = Trade(TradeID(1, EAITradeSystem), Day(2009, 1, 1), "cp",
    EAITradeAttributes(TreeID(1), TreeID(2), TreeID(3), "trader", "tradedfor", "broker", "clearinghouse"), ErrorInstrument("error1"))
  val option = new FuturesOption(Market.NYMEX_WTI, Day(2009, 8, 1), Month(2009, 9), Quantity(90, USD/BBL), Quantity(100, BBL), Call, European)

  @Test
  def strikeFieldShouldBeQuantity {
    TradeableFields.createFieldValues(trade, option ) should containEntries[Field, Any] (
      Field("Strike") -> option.strike
    )
  }
}