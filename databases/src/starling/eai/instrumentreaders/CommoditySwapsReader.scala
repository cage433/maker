package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.instrument.CommoditySwap
import starling.richdb.RichResultSetRow
import starling.quantity.{UOM, Quantity}
import starling.market.rules.{PerQuoteRule, PerFormulaRule}
import starling.daterange.{Day, DateRange}

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

    // From Ed Kennard - 7/10/11
    // This field is now always defaulted to ‘Per Quote’ for all swaps and derived trade types (e.g. FFAs, CFDs, Clearport Swaps),
    // where before it was defaulting to Per Formula.  This had to be hard-coded so it only takes effect for trades with a trade
    // date of 2011-09-07 or after, which is when this change was released, otherwise all historical trades would have been
    // updated causing all sorts of historical as well as current PnL changes.
    val roundingRule = if(rs.getDay("TradeDate") < Day(2011, 9, 7)) {
      PerFormulaRule
    } else {
      PerQuoteRule
    }

    CommoditySwap(index, strike, amount, averaging, cleared, rule, roundingMethodRule = roundingRule)
  }
}