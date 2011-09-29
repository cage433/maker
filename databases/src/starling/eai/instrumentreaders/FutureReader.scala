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
    val ccy = market.currency.inBaseCurrency
    val uom = market.uom
    val strike = Quantity(rs.getDouble("EntryPrice"), ccy / uom) inUOM market.priceUOM
    val lotSize = market.lotSize.get
    val amount = Quantity(rs.getDouble("Quantity") * lotSize, market.uom)

    Future(market, deliveryMonth, strike, amount)
  }
}