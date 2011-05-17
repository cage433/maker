package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.quantity.Quantity
import starling.instrument.{SwapCalendarSpread, CommoditySwap}
import starling.daterange.{SpreadPeriod, DateRange}
import starling.market.{SingleIndex, PublishedIndex}


class SwapCalendarSpreadReader extends InstrumentReader {

  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == SWAP_SPREAD

  val Formula = """"Dated Brent" ([-+]{1}) ([0-9.-]+)""".r

  override def create(rs: RichResultSetRow) = {
    val index = rs.getIndexFromEAIQuoteID("eaiquoteid")

//    val firstLeg = rs.getString("PricingFormulaFirstLeg")
//    val secondLeg = rs.getString("PricingFormulaSecondLeg")
//    require(secondLeg == "\"Dated Brent\"", "Unrecognised formula for second leg: " + (secondLeg, rs.getString("aspectid")))
//    val Formula(sign, value) = firstLeg
//    val strike = sign match {
//      case "+" => Quantity(value.toDouble, index.priceUOM)
//      case "-" => Quantity(value.toDouble * -1, index.priceUOM)
//    }
    val strike = Quantity(0, index.priceUOM) // until we understand the pricing xml

    val startFirst = rs.getDay("StartDate")
    val endFirst = rs.getDay("EndDate")

    val startSecond = rs.getDay("startdatesecondleg")
    val endSecond = rs.getDay("enddatesecondleg")

    val amount = rs.getQuantity("Quantity")
    val period = SpreadPeriod(DateRange(startFirst, endFirst), DateRange(startSecond, endSecond))

    index match {
      case si: SingleIndex => SwapCalendarSpread(si, strike, amount, period, cleared = true)
      case _ => throw new Exception("Can't have a SwapCalendarSpread on a spread index: " + index)
    }

  }
}