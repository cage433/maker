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
    val uom = amount.uom
    val marketCcy = index.priceUOM.numeratorUOM
    val eaiCCY = marketCcy.inBaseCurrency // EAI will have cents markets in USD
    // convert from usd to cent if we need to
    val strike = Quantity(rs.getDouble("FixedPrice"), eaiCCY / uom) inUOM (marketCcy / uom)
    val cleared = rs.getInt("TradeType") == CLEARPORT_SWAP
    val rule = if (cleared) {
      rs.getSwapPricingRule("PricingRule")
    } else {
      rs.getSwapPricingRule("PricingRule", "PricingRuleDefault")
    }
    val averaging = DateRange(start, end)

    CommoditySwap(index, strike, amount, averaging, cleared, rule)
  }
}