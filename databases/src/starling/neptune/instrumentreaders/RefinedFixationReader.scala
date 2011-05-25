package starling.neptune.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.market.NeptunePricingExchange
import starling.quantity.Quantity
import starling.instrument.{RefinedFixation, RefinedAssignment}

/**
 * Note that this doesn't extends InstrumentReader (which should really be TradeableReader) as it doesn't return a Tradeable
 */
object RefinedFixationReader{
  def create(row: RichResultSetRow) = {


    val exchangeCode = row.getString("EXCHANGE")
    val commodityCode = row.getString("MATERIALCODE")
    val market = NeptunePricingExchange.fromNeptuneCode(exchangeCode).inferMarketFromCommodityCode(commodityCode)
    val fixationDate = row.getDay("FIXATIONDATE")
    val purchaseOrSaleMultiplier = row.getString("PORS") match {
      case "P" => -1.0
      case "S" => 1.0
    }
    val fixationQuantity = Quantity(row.getDouble("FIXATIONQUANTITY") * purchaseOrSaleMultiplier,market.uom)
    val isAverage = row.getString("CONTRA_AVERAGE")

    RefinedFixation(market, fixationDate, isAverage, fixationQuantity)

  }

}
