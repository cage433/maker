package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.market.rules.{PerQuoteRule, PerFormulaRule}
import starling.daterange.{Day, DateRange}
import starling.quantity.{Percentage, Quantity}
import starling.quantity.UOM._
import starling.instrument.{FFA, CommoditySwap}

class FFAReader extends InstrumentReader {

  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == FFA_TRADE

  override def create(rs: RichResultSetRow) = {
    val index = rs.getSingleIndexFromEAIQuoteID("eaiquoteid")

    val start = rs.getDay("StartDate")
    val end = rs.getDay("EndDate")
    val averaging = DateRange(start, end)

    val amount = rs.getQuantity("Quantity")

    val strike = Percentage.fromPercentage(rs.getDouble("FixedPrice"))
    val fixedYear = rs.getYear("FixedLegFlatRateYear")
    val fixedRate = rs.getDoubleOption("FixedLegFlatRate").map(Quantity(_, USD/MT))

    val floatingYear = rs.getYear("FloatingLegFlatRateYear")

    val roundingOverride = rs.getIntOption("RoundingID")

    FFA(index, amount, strike, averaging, fixedYear, fixedRate, floatingYear, roundingOverride)
  }
}