package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.daterange.DateRange
import starling.richdb.RichResultSetRow
import starling.quantity.Quantity
import starling.calendar.BrentMonth
import starling.instrument.{CFD, CommoditySwap}
import starling.market.{Index, PublishedIndex, BrentCFDSpreadIndex}
import starling.market.formula.FormulaIndex

class CFDReader extends InstrumentReader {

  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == CFD_SWAP

  override def create(rs: RichResultSetRow) = {

    val index = rs.getString("CFDType") match {
      case "Dated Brent vs Platts Brent fixed month" => {
        val brentMonth = new BrentMonth(rs.getInt("BrentMonthID"))
        BrentCFDSpreadIndex.indexFor(brentMonth) match {
          case Some(i) => i
          case None => throw new Exception("Failed to create BrentCFDSpreadIndex: " + brentMonth)
        }
      }
      case "Urals Med vs Dated Brent" => {
        // Magic numer is the EAIQuoteID of "Urals Med (Platts) vs Dated Brent (Platts)"
        Index.indexFromEAIQuoteID(102).asInstanceOf[FormulaIndex]
      }
    }

    val start = rs.getDay("StartDate")
    val end = rs.getDay("EndDate")
    val strike = Quantity(rs.getDouble("FixedPrice"), index.priceUOM)
    val amount = rs.getQuantity("Quantity")
    val rule = rs.getSwapPricingRule("PricingRule", "PricingRuleDefault")

    CFD(index, strike, amount, DateRange(start, end), rule)
  }
}