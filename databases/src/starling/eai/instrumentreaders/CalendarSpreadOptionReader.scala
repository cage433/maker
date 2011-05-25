package starling.eai.instrumentreaders

import starling.richdb.RichResultSetRow
import starling.systemofrecord.InstrumentReader
import starling.daterange.Month
import starling.instrument.CalendarSpreadOption
import starling.daterange.{Spread, Month}
import starling.quantity.{UOM, Quantity}
import starling.utils.Log

class CalendarSpreadOptionReader extends InstrumentReader {
  import EAISystemOfRecord._

  override def canHandle(rs: RichResultSetRow) = {
    rs.getInt("TradeType") == ET_OPTION && rs.getString("optiontype") == CALENDAR_SPREAD_TYPE
  }

  override def create(rs: RichResultSetRow) = {
    val market = rs.getFuturesMarketFromEAIQuoteID("eaiquoteid")
    val amount = rs.getDouble("Quantity")

    val firstMonth = rs.getDay("ContractDate").containingMonth
    val secondMonth = Month(rs.getInt("spreadyear"), rs.getInt("SpreadMonthID"))

    val lotSize = market.lotSize.get
    val volume = Quantity(amount * lotSize, market.uom)

    val strike = Quantity(rs.getDouble("StrikePrice"), market.currency / market.uom)
    val callPut = rs.getCallPut("CallPut")
    CalendarSpreadOption(market, Spread(firstMonth, secondMonth), strike, volume, callPut)
  }
}
