package starling.neptune.instrumentreaders

import starling.richdb.RichResultSetRow
import starling.quantity.Quantity
import starling.instrument.RefinedAssignment
import starling.systemofrecord.InstrumentReader
import starling.market.{FuturesExchange, NeptuneCommodity}

object RefinedAssignmentReader extends InstrumentReader{
  def create(row: RichResultSetRow) : RefinedAssignment = {
    val exchangeCode = row.getString("EXCHANGE")
    val arbIntention = row.getString("ARB_IND")
    val riskExchangeCode = arbIntention match {
    // AFAIK this is done for copper only
      case "Y" => Map("LME" -> "CMX", "CMX" -> "LME")(exchangeCode)
      case "N" => exchangeCode
    }
    val commodityName = row.getString("COMMODITY_DESCRIPTION")
    val commodity = NeptuneCommodity.fromNeptuneName(commodityName).get
    val exchange = FuturesExchange.fromNeptuneCode(riskExchangeCode)
    val market = exchange.inferMarketFromCommodity(commodity).get
    val estimatedDeliveryDate = row.getDay("ASSIGNMENT_EST_DEL_DATE")
    val purchaseOrSaleMultiplier = row.getString("PORS") match {
      case "P" => 1.0
      case "S" => -1.0
    }
    val signedAssignmentAmount = row.getDouble("ASSIGNMENT_QTY") * purchaseOrSaleMultiplier
    val volume = Quantity(signedAssignmentAmount, market.uom)

    RefinedAssignment(market, estimatedDeliveryDate, volume)
  }

  def canHandle(rs: RichResultSetRow) = true
}