package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.quantity.Quantity
import starling.daterange.Day
import starling.instrument.{CashInstrumentType, CashInstrument, OrdinaryCost, Future}

class CostReader extends InstrumentReader {
  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == COST

  override def create(rs: RichResultSetRow) = {
    // TODO [25 May 2011] should add code and name
//    val name = rs.getString("costtype")
//    val code = rs.getString("suncostcode")

    val settlementDate = rs.getDay("tradedate")
    val quantity = rs.getQuantity("quantity", "currency")

    CashInstrument(CashInstrumentType.Ordinary, quantity, settlementDate, None, None)
  }
}