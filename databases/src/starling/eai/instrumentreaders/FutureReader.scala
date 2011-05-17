package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.quantity.{Quantity, UOM}
import starling.instrument.Future
import starling.utils.Log

class FutureReader extends InstrumentReader {
  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == FUTURE

  override def create(rs: RichResultSetRow) = {
    val market = rs.getFuturesMarketFromEAIQuoteID("eaiquoteid")
    val deliveryMonth = rs.getDay("ContractDate").containingMonth
    val strike = rs.getDouble("EntryPrice")
    val lotSize = market.lotSize.get
    val amount = Quantity(rs.getDouble("Quantity") * lotSize, market.uom)

    Future(market, deliveryMonth, Quantity(strike, market.currency / market.uom), amount)
  }
}