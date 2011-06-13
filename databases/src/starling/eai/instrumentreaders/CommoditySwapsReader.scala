package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.daterange.DateRange
import starling.instrument.CommoditySwap
import starling.richdb.RichResultSetRow
import starling.quantity.{UOM, Quantity}


class CommoditySwapsReader extends InstrumentReader {

  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == SWAP || rs.getInt("TradeType") == CLEARPORT_SWAP

  override def create(rs: RichResultSetRow) = {
    val index = rs.getIndexFromEAIQuoteID("eaiquoteid")
    val start = rs.getDay("StartDate")
    val end = rs.getDay("EndDate")
    val amount = rs.getQuantity("Quantity")
    val ccy = index.priceUOM.numeratorUOM
    val strike = Quantity(rs.getDouble("FixedPrice"), ccy / amount.uom)
    val cleared = rs.getInt("TradeType") == CLEARPORT_SWAP
    val rule = if (cleared) {
      rs.getSwapPricingRule("PricingRule")
    } else {
      rs.getSwapPricingRule("PricingRule", "PricingRuleDefault")
    }
    val averaging = index.makeAveragingPeriodMonthIfPossible(DateRange(start, end), rule)

    CommoditySwap(index, strike, amount, averaging, cleared, rule)
  }
}