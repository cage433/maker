package starling.services.trade

import starling.instrument.TradeAttributes
import starling.tradestore.eai.EAITradeAttributes
import starling.eai.{EAIStrategyDB, TreeID}
import starling.eai.instrumentreaders.EAISystemOfRecord
import starling.tradestore.intraday.IntradayTradeAttributes

class ExcelTradeAttributes(attributes: Map[String, Any]) extends TradeAttributes {
  def details = attributes.map {
    case (k, v) => ExcelTradeAttributes.normalForm(k) -> v
  }
}

object ExcelTradeAttributes {
  def normalForm(s: String): String = s.filterNot(_ == ' ').toLowerCase
}
