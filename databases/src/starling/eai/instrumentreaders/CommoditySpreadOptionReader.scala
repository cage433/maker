package starling.eai.instrumentreaders

import starling.richdb.RichResultSetRow
import starling.systemofrecord.InstrumentReader
import starling.daterange.{Spread, Month}
import starling.quantity.Quantity
import starling.market.FuturesSpreadMarket
import starling.instrument.{CommoditySpreadOption}

class CommoditySpreadOptionReader extends InstrumentReader {
  import EAISystemOfRecord._

  override def canHandle(rs: RichResultSetRow) = {
    rs.getInt("TradeType") == ET_OPTION && (try {
      rs.getFuturesMarketFromEAIQuoteID("eaiquoteid") match {
        case fsm: FuturesSpreadMarket => true
        case _ => false
      }
    } catch {
      case _ => false
    })
  }

  override def create(rs: RichResultSetRow) = {
    val market = rs.getFuturesMarketFromEAIQuoteID("eaiquoteid").asInstanceOf[FuturesSpreadMarket]
    val amount = rs.getDouble("Quantity")

    val month = rs.getDay("ContractDate").containingMonth

    val lotSize = market.lotSize.get
    val volume = Quantity(amount * lotSize, market.uom)

    val strike = Quantity(rs.getDouble("StrikePrice"), market.currency / market.uom)
    val callPut = rs.getCallPut("CallPut")
    CommoditySpreadOption(market, month, strike, volume, callPut)
  }
}
